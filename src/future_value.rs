//! Run expensive computations in a background thread.

use std::sync::mpsc::{channel, Receiver};

/// Represents a value that may still be computed in a background thread.
#[derive(Default)]
pub(crate) enum FutureValue<T: ComputableValue> {
    /// The value is currently uninitialized and thus not available.
    #[default]
    Uninitialized,
    /// The value was previously computed, but may not be up to date.
    Value(T),
    /// The computation of the value is ongoing in a background thread.
    ///
    /// Note that the resulting value may not be up to date anymore when its computation is
    /// finished.
    OngoingComputation {
        /// The receiver that will receive the value when the computation is finished.
        receiver: Receiver<T>,
    },
}

impl<T: ComputableValue + Send + 'static> FutureValue<T> {
    /// Checks if the given value is up to date.
    pub(crate) fn is_current(&self, ctx: T::CheckCtx<'_>) -> bool {
        match self {
            FutureValue::Uninitialized => false,
            FutureValue::Value(val) => val.is_current(ctx),
            FutureValue::OngoingComputation { .. } => false,
        }
    }

    /// Checks if an update needs to occur.
    ///
    /// This also tries to receive the value from the background thread.
    pub(crate) fn update_check(&mut self, ctx: T::CheckCtx<'_>) -> bool {
        match self {
            FutureValue::Uninitialized => true,
            FutureValue::Value(val) => !val.is_current(ctx),
            FutureValue::OngoingComputation { receiver } => match receiver.try_recv() {
                Ok(val) if val.is_current(ctx) => {
                    *self = Self::Value(val);

                    false
                }
                Ok(_) => true,
                Err(_) => false,
            },
        }
    }

    /// Starts a new computation of the value in a background thread.
    pub(crate) fn update(&mut self, ctx: T::ComputeCtx, on_finish: impl FnOnce() + Send + 'static) {
        let (sender, receiver) = channel();

        std::thread::spawn(move || {
            sender.send(T::compute(ctx)).ok();
            on_finish();
        });

        *self = Self::OngoingComputation { receiver };
    }

    /// Returns the computed value.
    pub(crate) fn get(&self) -> Option<&T> {
        match self {
            Self::Value(val) => Some(val),
            _ => None,
        }
    }
}

/// Represents something that can be computed in a background thread.
pub(crate) trait ComputableValue {
    /// The context required to check if a value is up to date.
    type CheckCtx<'check>;

    /// The context required to compute a new value.
    type ComputeCtx: Send;

    /// Returns `true` if the value is up to date.
    fn is_current(&self, ctx: Self::CheckCtx<'_>) -> bool;

    /// Computes a new value.
    ///
    /// This will be run in a background thread if the value gets stale.
    fn compute(ctx: Self::ComputeCtx) -> Self;
}

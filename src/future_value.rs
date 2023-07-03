use std::sync::mpsc::{channel, Receiver};

#[derive(Default)]
pub(crate) enum FutureValue<T: ComputableValue> {
    #[default]
    Uninitialized,
    Value(T),
    OngoingComputation {
        receiver: Receiver<T>,
    },
}

impl<T: ComputableValue + Send + 'static> FutureValue<T> {
    fn start_computation(&mut self, ctx: T::ComputeCtx, on_finish: Box<dyn FnOnce() + Send>) {
        let (sender, receiver) = channel();

        std::thread::spawn(move || {
            sender.send(T::compute(ctx)).ok();
            on_finish();
        });

        *self = Self::OngoingComputation { receiver };
    }

    pub(crate) fn is_current(&self, ctx: &T::CheckCtx) -> bool {
        match self {
            FutureValue::Uninitialized => false,
            FutureValue::Value(val) => val.is_current(ctx),
            FutureValue::OngoingComputation { .. } => false,
        }
    }

    pub(crate) fn update_check(&mut self, ctx: &T::CheckCtx) -> bool {
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

    pub(crate) fn update(&mut self, ctx: T::ComputeCtx, on_finish: impl FnOnce() + Send + 'static) {
        self.start_computation(ctx, Box::new(on_finish));
        /*
        match std::mem::take(self) {
            Self::Value(val) if val.is_current(&ctx) => *self = Self::Value(val),
            Self::Value(_) | Self::Uninitialized => {
                self.start_computation(ctx, Box::new(on_finish));
            }
            Self::OngoingComputation {
                receiver,
                on_finish: prev_on_finish,
            } => match receiver.try_recv() {
                Ok(val) if val.is_current(&ctx) => {
                    *self = Self::Value(val);
                    prev_on_finish();
                }
                Ok(_) => self.start_computation(ctx, Box::new(on_finish)),
                Err(_) => {
                    *self = Self::OngoingComputation {
                        receiver,
                        on_finish: prev_on_finish,
                    }
                }
            },
        }
        */
    }

    pub(crate) fn get(&self) -> Option<&T> {
        match self {
            Self::Value(val) => Some(val),
            _ => None,
        }
    }
}

pub(crate) trait ComputableValue {
    type CheckCtx;
    type ComputeCtx: Send;

    fn is_current(&self, ctx: &Self::CheckCtx) -> bool;

    fn compute(ctx: Self::ComputeCtx) -> Self;
}

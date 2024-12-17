use std::collections::HashMap;

pub mod compiler_functions;
pub mod functions;
pub mod structs;
pub mod types;
pub mod variable;

pub trait MsRegistry<T> {
    fn get_registry(&self) -> &HashMap<String, T>;
    fn get_registry_mut(&mut self) -> &mut HashMap<String, T>;
}

pub trait MsRegistryExt<T>
where
    Self: MsRegistry<T>,
{
    fn get(&self, s: &str) -> Option<&T> {
        self.get_registry().get(s)
    }

    fn add(&mut self, k: String, v: T) -> Option<()> {
        if self.get_registry_mut().insert(k, v).is_none() {
            Some(())
        } else {
            None
        }
    }
}

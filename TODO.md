# TODO



```(defn reset-weapon-counters
  [self required state]
  (-> state
      (assoc-in [:entities :data self :attacks :fireball :attacks :counter] 0)
      (assoc-in [:entities :data self :attacks :fireball :attacks :cooldown] 0)
      (assoc-in [:entities :data self :attacks :firewall :attacks :counter] 0)
      (assoc-in [:entities :data self :attacks :firewall :attacks :cooldown] 0)))```
      
A type of definition as above requires the usage of very specific paths. A possible way to address
this would be to implement a function like `set-self` which would provide a partial function with the
path to the entity data mapped in 

Similar approach could be implemented for updates.

```(update-in [:entities :data self :attacks :firewall :attacks :counter] inc)```

Could be replaced with `update-self`

The `self` parameter for evaluations currently provides the id

It could be rewritten to provide the whole data structure, from which the id could be extracted if needed.


An update to the way the entity-state is handled in two separate pieces should be considered. Currently,
the engine contains a separate state object for entity data - positions, texture etc. and the logic system 
contains a wider set of data. If an entity updates in logic, it has to be updated also in engine. There is 
some complexity with regards of this is done currently, unnecessarily. 

The implementation of the engine is intended to be a simple view. 

What could be done is an option to provide a separate entity-state object to the engine, from which the 
engine would be capable of rendering current scene. What is not wanted is add a dependency to tila, if
it can be avoided. The two modules should be separate.

It could be done that an atom woul

A partial fork of Lars Petersen's [HiggsSet](https://github.com/lpeterse/HiggsSet). Unless you're looking for one of the very specific advantages I'm going for here, you're probably better off looking at HiggsSet.

JaggedSet is a multi-index set for sum types desgined to enforce relations on update. You put all of your data in one sum type and then use constructor-specific instances to map related fields onto shared indexes in a JaggedSet. Every record now stores every field related to a given constructor so you don't need to JOIN on every request. Furthermore, when you update an index,  you also update its value in every record it points to, enforcing relations between constructors in a simple fashion.

As you may have guessed, this strategy is most beneficial for highly read-biased workloads. Furthermore, types of index values are limited - currently to bytestrings and ints.

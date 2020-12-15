package com.neopragma.cobolcheck;

public interface Tuple<T1, T2> {
    T1 getFirst();
    T2 getSecond();
    boolean isEmpty();
}

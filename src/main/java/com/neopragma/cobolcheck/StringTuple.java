package com.neopragma.cobolcheck;

public class StringTuple implements Tuple {
    private final String first;
    public final String second;
    public StringTuple(String first, String second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public String getFirst() { return first; }
    @Override
    public String getSecond() { return second; }
    @Override
    public boolean isEmpty() { return first == null; }
}

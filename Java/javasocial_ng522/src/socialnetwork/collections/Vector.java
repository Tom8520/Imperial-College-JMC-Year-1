package socialnetwork.collections;

import java.util.ArrayList;
import java.util.List;

public class Vector<E> {

  E[] elements;

  private final static int INITIAL_SIZE = 100;

  private final static int MAX_SIZE = INITIAL_SIZE * 1000;

  private int size = 0;
  private int maxSize = 0;

  public Vector() {
    resize();
  }

  private void resize() {
    E[] old = elements;
    maxSize += INITIAL_SIZE;
    elements = (E[]) new Object[maxSize];
    if (size > 0) {
      System.arraycopy(old, 0, elements, 0, size);
    }
  }

  public E get(int index) {
    assert index < size && index >= 0 : "Index out of bounds";
    return elements[index];
  }

  public boolean add(E element) {
    if (size == MAX_SIZE) {
      return false;
    }
    if (size == maxSize) {
      resize();
    }
    elements[size] = element;
    size++;

    return true;
  }

  public boolean remove(E element) {
    assert size > 0;
    int i = 0;

    boolean found = false;

    for (; i < size; i++) {
      if (elements[i].equals(element)) {
        found = true;
        break;
      }
    }

    for (i++; i < size; i++) {
      elements[i - 1] = elements[i];
    }

    if (found) {
      size--;
    }
    return found;
  }

  public int size() {
    return size;
  }

  public boolean isEmpty() {
    return size == 0;
  }

  @Override
  public String toString() {
    String s = "[";
    for (int i = 0; i < size; i++) {
      s += elements[i].toString();
      if (i < size - 1) {
        s += ", ";
      }
    }
    return s + "]";
  }

  public List<E> toList() {
    List<E> l = new ArrayList<>();
    for (int i = 0; i < size; i++) {
      l.add(elements[i]);
    }
    return l;
  }
}

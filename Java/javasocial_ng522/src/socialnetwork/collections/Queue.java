package socialnetwork.collections;

public class Queue<E> extends Vector<E> {

  public Queue() {
    super();
  }

  public boolean enqueue(E element) {
    return add(element);
  }

  public E dequeue() {
    E element = get(0);
    remove(element);
    return element;
  }
}

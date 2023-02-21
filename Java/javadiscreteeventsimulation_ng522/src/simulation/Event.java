package simulation;

public interface Event<S> {

  void invoke(S simulation);
}

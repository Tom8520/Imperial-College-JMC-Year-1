package socialnetwork.domain;

import java.util.Optional;
import socialnetwork.collections.Queue;

public class TaskBacklog implements Backlog {

  Queue<Task> tasks = new Queue<>();

  @Override
  synchronized public boolean add(Task task) {
    return tasks.enqueue(task);
  }

  @Override
  synchronized public Optional<Task> getNextTaskToProcess() {
    if (tasks.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(tasks.dequeue());
  }

  @Override
  synchronized public int numberOfTasksInTheBacklog() {
    return tasks.size();
  }
}

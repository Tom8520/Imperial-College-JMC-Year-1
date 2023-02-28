package socialnetwork.domain;

import java.util.Optional;
import socialnetwork.domain.Task.Command;

public class Worker extends Thread {

  private final Backlog backlog;
  private boolean interrupted = false;

  private final static int SLEEP_TIME = 10;

  public Worker(Backlog backlog) {
    this.backlog = backlog;
  }

  @Override
  public void run() {
    while (!interrupted) {
      Optional<Task> task = backlog.getNextTaskToProcess();

      if (task.isEmpty()) {
        try {
          Thread.sleep(SLEEP_TIME);
        } catch (InterruptedException e) {
          throw new RuntimeException(e);
        }
        continue;
      }

      process(task.get());
    }
  }

  public void interrupt() {
    this.interrupted = true;
  }

  public void process(Task nextTask) {
    if (nextTask.getCommand() == Command.DELETE) {
      if (!nextTask.getBoard().deleteMessage(nextTask.getMessage())) {
        backlog.add(nextTask);
      }
    } else {
      nextTask.getBoard().addMessage(nextTask.getMessage());
    }
  }
}

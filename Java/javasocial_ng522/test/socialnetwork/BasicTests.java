package socialnetwork;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import org.junit.Test;
import socialnetwork.domain.Backlog;
import socialnetwork.domain.Board;
import socialnetwork.domain.Message;
import socialnetwork.domain.Task;
import socialnetwork.domain.Task.Command;
import socialnetwork.domain.TaskBacklog;
import socialnetwork.domain.UserBoard;
import socialnetwork.domain.Worker;

public class BasicTests {

  @Test
  public void testRegistration() {
    // TODO replace null here by your implementation of Board and Backlog
    Backlog backlog = new TaskBacklog();
    Board board = new UserBoard();
    SocialNetwork socialNetwork = new SocialNetwork(backlog);
    User user = new User("test", socialNetwork);
    socialNetwork.register(user, board);

    assertTrue(socialNetwork.getBoards().containsKey(user));
    assertEquals(socialNetwork.getBoards().get(user), board);
  }

  @Test
  public void testMessageLifecycle() {
    // TODO replace null here by your implementation of Board and Backlog
    Backlog backlog = new TaskBacklog();
    Board board1 = new UserBoard();
    Board board2 = new UserBoard();
    Board board3 = new UserBoard();
    SocialNetwork socialNetwork = new SocialNetwork(backlog);
    User user1 = new User("test1", socialNetwork);
    User user2 = new User("test2", socialNetwork);
    User user3 = new User("test3", socialNetwork);
    socialNetwork.register(user1, board1);
    socialNetwork.register(user2, board2);
    socialNetwork.register(user3, board3);

    Message sent = socialNetwork.postMessage(user1, Arrays.asList(user2, user3), "cats > dogs");
    assertEquals(3, backlog.numberOfTasksInTheBacklog());

    // the sender also receives his own message
    Optional<Task> ot1 = backlog.getNextTaskToProcess();
    Optional<Task> ot2 = backlog.getNextTaskToProcess(); // to user2
    Optional<Task> ot3 = backlog.getNextTaskToProcess(); // to user3

    assertTrue(ot1.isPresent());
    assertTrue(ot2.isPresent());
    assertTrue(ot3.isPresent());
    assertEquals(0, backlog.numberOfTasksInTheBacklog());

    Task t1 = ot1.get();
    Task t2 = ot2.get();
    Task t3 = ot3.get();

    Set<Board> targetBoards = new HashSet<>();
    targetBoards.add(board1);
    targetBoards.add(board2);
    targetBoards.add(board3);

    assertEquals(t1.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t1.getCommand(), Command.POST);
    assertTrue(targetBoards.remove(t1.getBoard()));

    assertEquals(t2.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t2.getCommand(), Command.POST);
    assertTrue(targetBoards.remove(t2.getBoard()));

    assertEquals(t3.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t3.getCommand(), Command.POST);
    assertTrue(targetBoards.remove(t3.getBoard()));

    Worker worker = new Worker(backlog);
    worker.process(t1);
    worker.process(t2);
    worker.process(t3);

    assertTrue(board1.getBoardSnapshot().contains(sent));
    assertTrue(board2.getBoardSnapshot().contains(sent));
    assertTrue(board3.getBoardSnapshot().contains(sent));

    // now let's delete the message
    socialNetwork.deleteMessage(sent);

    assertEquals(3, backlog.numberOfTasksInTheBacklog());
    ot2 = backlog.getNextTaskToProcess(); // to user2
    ot3 = backlog.getNextTaskToProcess(); // to user3
    ot1 = backlog.getNextTaskToProcess(); // to user1

    assertTrue(ot1.isPresent());
    assertTrue(ot2.isPresent());
    assertTrue(ot3.isPresent());
    assertEquals(0, backlog.numberOfTasksInTheBacklog());

    t1 = ot1.get();
    t2 = ot2.get();
    t3 = ot3.get();

    targetBoards.add(board1);
    targetBoards.add(board2);
    targetBoards.add(board3);

    assertEquals(t1.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t1.getCommand(), Command.DELETE);
    assertTrue(targetBoards.remove(t1.getBoard()));

    assertEquals(t2.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t2.getCommand(), Command.DELETE);
    assertTrue(targetBoards.remove(t2.getBoard()));

    assertEquals(t3.getMessage().getMessageId(), sent.getMessageId());
    assertEquals(t3.getCommand(), Command.DELETE);
    assertTrue(targetBoards.remove(t3.getBoard()));

    worker.process(t1);
    worker.process(t2);
    worker.process(t3);

    assertTrue(board1.getBoardSnapshot().isEmpty());
    assertTrue(board2.getBoardSnapshot().isEmpty());
    assertTrue(board3.getBoardSnapshot().isEmpty());
  }
}

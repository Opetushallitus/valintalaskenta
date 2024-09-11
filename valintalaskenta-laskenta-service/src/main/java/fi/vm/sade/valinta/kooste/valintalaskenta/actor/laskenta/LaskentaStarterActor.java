package fi.vm.sade.valinta.kooste.valintalaskenta.actor.laskenta;

import akka.actor.Props;
import akka.actor.UntypedActor;
import fi.vm.sade.valinta.kooste.valintalaskenta.actor.LaskentaSupervisor;
import java.util.concurrent.atomic.AtomicInteger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LaskentaStarterActor extends UntypedActor {
  private final Logger LOG = LoggerFactory.getLogger(LaskentaStarterActor.class);

  private final LaskentaSupervisor laskentaSupervisor;
  private int maxWorkers;
  private AtomicInteger workerCount = new AtomicInteger(0);

  private LaskentaStarterActor(final LaskentaSupervisor laskentaSupervisor, final int maxWorkers) {
    this.laskentaSupervisor = laskentaSupervisor;
    this.maxWorkers = maxWorkers;
    LOG.info("Creating LaskentaStarterActor with maxWorkerCount {}", maxWorkers);
  }

  public static Props props(final LaskentaSupervisor laskentaSupervisor, final int maxWorkers) {
    return Props.create(
        LaskentaStarterActor.class, () -> new LaskentaStarterActor(laskentaSupervisor, maxWorkers));
  }

  @Override
  public void onReceive(Object message) {
    if (WorkAvailable.class.isInstance(message)) {
      startLaskentaIfWorkersAvailable();
    } else if (StartAllWorkers.class.isInstance(message)) {
      LOG.info("Starting all workers. Current workerCount: " + workerCount.get());
      while (workerCount.get() < maxWorkers) {
        startLaskentaIfWorkersAvailable();
      }
    } else if (WorkerAvailable.class.isInstance(message)) {
      decrementWorkerCount();
      startLaskentaIfWorkersAvailable();
    } else if (NoWorkAvailable.class.isInstance(message)) {
      decrementWorkerCount();
    } else if (ResetWorkerCount.class.isInstance(message)) {
      workerCount.set(0);
      LOG.info("Worker count reset: " + workerCount.get());
    } else if (MaxWorkerCount.class.isInstance(message)) {
      MaxWorkerCount count = (MaxWorkerCount) message;
      LOG.info("Set maxworker count to " + count.maxWorkerCount);
      maxWorkers = count.maxWorkerCount;
    } else {
      LOG.error("Unknown message: " + message);
    }
  }

  private void startLaskentaIfWorkersAvailable() {
    int wasNumberOfWorkers =
        workerCount.getAndUpdate(
            current -> {
              if (current < maxWorkers) {
                return ++current;
              } else {
                return current;
              }
            });
    LOG.info("Process; maxWorkers: {}, workerCount: {}", maxWorkers, wasNumberOfWorkers);
    if (wasNumberOfWorkers < maxWorkers) { // if it was less than maxWorkers then it was incremented
      LOG.info("Reserving a new worker, workerCount: {}", (wasNumberOfWorkers + 1));
      laskentaSupervisor.fetchAndStartLaskenta();
    }
  }

  public int getWorkerCount() {
    return workerCount.get();
  }

  private void decrementWorkerCount() {
    int workerCount = this.workerCount.updateAndGet(i -> i > 0 ? i - 1 : i);
    LOG.info("Releasing worker, workerCount: {}", workerCount);
  }

  public static class WorkAvailable {}

  public static class NoWorkAvailable {}

  public static class WorkerAvailable {}

  public static class ResetWorkerCount {}

  public static class StartAllWorkers {}

  public static class MaxWorkerCount {
    public final int maxWorkerCount;

    public MaxWorkerCount(int count) {
      this.maxWorkerCount = count;
    }
  }
}

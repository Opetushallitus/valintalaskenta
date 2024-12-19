package fi.vm.sade.valintalaskenta.runner.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import fi.vm.sade.valintalaskenta.config.AwsConfiguration;
import fi.vm.sade.valintalaskenta.runner.service.EcsTaskManager;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import software.amazon.awssdk.services.ecs.EcsClient;
import software.amazon.awssdk.services.ecs.model.UpdateTaskProtectionRequest;

@Service
public class EcsTaskManagerImpl implements EcsTaskManager {

  private static final Logger LOG = LoggerFactory.getLogger(EcsTaskManagerImpl.class);

  public static final int TASK_PROTECTION_LENGHT_MINUTES = 3;

  private final EcsClient ecsClient;
  private final AtomicInteger counter = new AtomicInteger(0);
  private final ExecutorService executor = Executors.newSingleThreadExecutor();
  private final String clusterArn;
  private final String taskArn;
  private final boolean isLocal;

  public EcsTaskManagerImpl(EcsClient ecsClient, Environment environment) {
    this.ecsClient = ecsClient;

    this.isLocal = AwsConfiguration.isLocal(environment);

    if (isLocal) {
      this.clusterArn = "CLUSTER ARN";
      this.taskArn = "TASK ARN";
    } else {
      RestTemplate restTemplate = new RestTemplate();
      String ecsMetadataUrlV4 = System.getenv().get("ECS_CONTAINER_METADATA_URI_V4");
      ResponseEntity<String> response =
          restTemplate.getForEntity(ecsMetadataUrlV4 + "/task", String.class);

      ObjectMapper mapper = new ObjectMapper();
      try {
        Map<String, Object> data = mapper.readValue(response.getBody(), Map.class);
        this.clusterArn = data.get("Cluster").toString();
        this.taskArn = data.get("TaskARN").toString();
      } catch (Exception e) {
        throw new RuntimeException(e);
      }
    }
  }

  @Override
  public String getTaskArn() {
    return this.taskArn;
  }

  private void extendTaskProtection() {
    LOG.info(
        "Extending task protection for task {} for {} minutes",
        this.taskArn,
        TASK_PROTECTION_LENGHT_MINUTES);
    if (!isLocal) {
      this.ecsClient.updateTaskProtection(
          UpdateTaskProtectionRequest.builder()
              .cluster(this.clusterArn)
              .tasks(taskArn)
              .protectionEnabled(true)
              .expiresInMinutes(TASK_PROTECTION_LENGHT_MINUTES)
              .build());
    }
  }

  @Scheduled(initialDelay = 15, fixedDelay = 60, timeUnit = TimeUnit.SECONDS)
  public void extendTaskProtectionPeriodically() {
    this.executor.submit(
        () -> {
          if (this.counter.get() > 0) {
            this.extendTaskProtection();
          }
        });
  }

  @Override
  public void withTaskProtection(Runnable runnable) {
    int value = counter.incrementAndGet();
    try {
      if (value == 1) {
        this.extendTaskProtection();
      }

      runnable.run();
    } finally {
      counter.decrementAndGet();
    }
  }
}

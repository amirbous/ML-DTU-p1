library(torch)

train_neural_net <- function(model, loss_fn, X, y, n_replicates = 3,
                             max_iter = 10000, tolerance = 1e-6) {
  best_final_loss <- 1e100
  logging_frequency <- 1000

  for (r in seq_len(n_replicates)) {
    cat("\n\tReplicate: ", r, "/", n_replicates, "\n")

    net <- model()

    nn_init_xavier_uniform_(net$parameters$"0.weight")
    nn_init_xavier_uniform_(net$parameters$"2.weight")

    # We can optimize the weights by means of stochastic gradient descent
    optimizer <- optim_adam(net$parameters, lr = 0.01)

    learning_curve <- c()
    old_loss <- 1e6
    for (i in seq_len(max_iter)) {
      y_est <- net(X)
      loss <- loss_fn(y_est, y)

      loss_value <- loss$item()
      learning_curve <- c(learning_curve, loss_value)

      p_delta_loss <- abs(loss_value - old_loss) / old_loss
      if (p_delta_loss < tolerance) {
        break
      }
      old_loss <- loss_value

      if (i != 1 && i %% logging_frequency == 0) {
        cat("\t\tIteration: ", i, "/", max_iter, " loss: ", loss_value, "\n")
      }

      optimizer$zero_grad()
      loss$backward()
      optimizer$step()
    }

    if (loss_value < best_final_loss) {
      best_final_loss <- loss_value
      best_net <- net
      best_learning_curve <- learning_curve
    }
  }

  return(list(
    net = best_net,
    final_loss = best_final_loss,
    learning_curve = best_learning_curve
  ))
}

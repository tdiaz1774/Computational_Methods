Tomas Diaz Servin A01637531

Mateo González Cosío Ríos y Valles A01023938

Jorge Cabiedes Acosta A01024053
# Activity 5.3: Parallel Syntax Lexor
# Execution Times

## Sequential                                                                                  
#### AVG: 2.629
| TRIAL     | TIME       |
|-----------|------------|
| 1.        | 2.621 ms   |
| 2.        | 2.626 ms   |
| 3.        | 2.582 ms   |   
| 4.        | 2.663 ms.  |
| 5.        | 2.647 ms.  |
 
## Parallel                                                                              
#### AVG: 2.604
| TRIAL     | TIME       |
|-----------|------------|
| 1.        | 2.616 ms   |
| 2.        | 2.613 ms   |
| 3.        | 2.582 ms   |   
| 4.        | 2.612 ms.  |
| 5.        | 2.600 ms.  |
 
## Comparison
In the case of this algorithm, when we compare compilation times of both algorithms we can observe a **0.99** speedup when using the parallel algorithm with **16 cores** compared to its sequential version.
Although it may not appear as a large difference, it is worth noting that even a small increment in time can mean a huge gap in results when using heavier ammounts of data with larger tasks.

  
# Complexity

In the case of both algorithms, we can determine that they both have a complexity of **O(N)**. This is due to the fact that execution times and space the algorithm takes up follows a **linear tendency**, meaning that as the algorithm is asked to analyze more data, exextution times will also increase. 
For our parallel algorithm it is worthy to note that the complexity is unaffected due to the fact **parallelism does not affect the comoplexity of an algorithm**, as it only divides the task between multiple cores which causes faster execution times.

# Conclusion

The solution of this activity was farily straightforward, as it took the work of an already completed algorithm and that's only major change was to implement parallelism to the solution.
Using a predetermined function, we adapted the sequential algorthm to have the ability to read multiple files at the same time using futures from racket. With this we were able to process a large ammount of data 
in a decreased ammount of time by distributing the workload by the ammount of futures which would best suit our computers.

Parallelism is an extremely useful tool, as it allows us to work efficiently with large ammounts of data which could be used for the solution of many problems. This however, means that it could also be used maliciously as it gives a lot of power to author which could use it wrongly depending of his/her intentions. It could be used to guess passwords, breach security or any wide array of choices if not used ethically.
It is important for us as programmers to use it carefully and ethically as it could have painful implications to the users of many services that handle sensitive infromation which lays vulnerable if connected to the internet and not protected carefully.

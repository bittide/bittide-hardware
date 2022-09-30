| Address    | Length  | What                         |
|------------|---------|------------------------------|
| 0x00000000 | 32K     | Instruction memory           |
| 0x00008000 | 4K      | FDT                          |
| 0x00009000 | 28K     | Data memory                  |
| 0x00010000 | 32K     | Scatter memory               |
| 0x00018000 | 32K     | Gather  memory               |
| 0x00020000 | 32B(1K) | rx Unit                      |
| 0x00020400 | 8K      | Scatter unit calendar        |
| 0x00022400 | 1B (1K) | tx Unit                      |
| 0x00022800 | 8K      | Gather unit calendar         |
| 0x00030000 | 8K      | Switch calendar              |
| 0x00032000 | 32B(1K) | Link 0 rx Unit               |
| 0x00032400 | 1B (1K) | Link 0 tx Unit               |
| 0x00040000 | 32B(1K) | GPPE 0 rx Unit               |
| 0x00040400 | 8K      | GPPE 0 Scatter unit calendar |
| 0x00042400 | 1B (1K) | GPPE 0 tx Unit               |
| 0x00042800 | 8K      | GPPE 0 Gather unit calendar  |
| 0x00050000 | 32B(1K) | GPPE 1 rx Unit               |
| 0x00050400 | 8K      | GPPE 1 Scatter unit calendar |
| 0x00052400 | 1B (1K) | GPPE 1 tx Unit               |
| 0x00052800 | 8K      | GPPE 1 Gather unit calendar  |

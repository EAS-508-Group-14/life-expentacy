## Regression Comparison

### R^2

| Regression | SLR       |       MLR | SVR       |       GPR | RandomForest |
| ---------: | :-------- | --------: | :-------- | --------: | :----------- |
|  With Year | 0.0015316 |  0.553165 | 0.6661983 | 0.9366472 | 0.9900383    |
|   W/O Year | 0.4936722 | 0.6146749 | 0.5878877 | 0.9362345 | 0.9900376    |

### RMSE Ratio

| Regression | SLR       |       MLR | SVR       |       GPR | RandomForest |
| ---------: | :-------- | --------: | :-------- | --------: | :----------- |
|  With Year | 0.9540329 | 1.0096760 | 0.5430761 | 0.6383334 | 0.4583040    |
|   W/O Year | 0.9514942 | 0.9326662 | 0.6016517 | 0.6264042 | 0.4056406    |

### Descriptor's Importance after VIP process

- **With Year**: Total.expenditure, BMI, under.five.deaths, Polio, Population, Schooling
- **W/O Year**: Adult.Mortality, Polio, under.five.deaths, Measles, Population, HIV.AIDS

### Conclusion

It seems that column `Year` is not much impact to the data. Although **Life Expectency** has increased every year but the result show that the consequences are other factors.

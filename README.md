# Disease Prediction System

## Overview

The Disease Prediction System is an AI/ML-powered application that predicts the likelihood of heart disease and diabetes based on user input parameters. This project uses machine learning algorithms implemented in R with a Shiny web interface to provide real-time predictions and visualizations.

## Features

- **Heart Disease Prediction**: Uses Naive Bayes algorithm to predict heart disease risk based on 13 medical parameters
- **Diabetes Prediction**: Employs machine learning to assess diabetes risk using 8 health indicators
- **Interactive Web Interface**: Built with R Shiny for easy user interaction
- **Real-time Visualization**: Displays prediction probabilities with interactive charts
- **Dual Prediction System**: Simultaneously predicts both conditions for comprehensive health assessment

## Technologies Used

- **R**: Core programming language
- **Shiny**: Web application framework for R
- **e1071**: Support Vector Machines and Naive Bayes implementation
- **ggplot2**: Data visualization library
- **Machine Learning**: Naive Bayes classification algorithm

## Prerequisites

Before running this application, ensure you have the following installed:

- R (version 3.6 or higher)
- Required R packages:
  - shiny
  - e1071
  - ggplot2

## Installation

1. Clone the repository:
```bash
git clone https://github.com/Nirajlpu/disease-prediction-system.git
cd disease-prediction-system
```

2. Install required R packages:
```r
install.packages(c("shiny", "e1071", "ggplot2"))
```

3. Ensure the dataset files are in the project directory:
   - `HeartDisease.csv`
   - `DiabetesData.csv`

## Usage

1. Run the application:
```r
source("project.R")
```

2. The Shiny web application will open in your default browser

3. Navigate between the "Heart Disease" and "Diabetes" tabs to input relevant medical parameters

4. Click the "Submit" button to get predictions and view the probability chart

## Input Parameters

### Heart Disease Prediction
- Age (1-120 years)
- Gender (1 = Male, 0 = Female)
- Chest Pain Type (1-4)
- Resting Blood Pressure
- Cholesterol Level
- Fasting Blood Sugar (1 = >120 mg/dl, 0 = ≤120 mg/dl)
- Resting Electrocardiographic Results (0, 1, 2)
- Maximum Heart Rate
- Exercise Induced Angina (1 = Yes, 0 = No)
- ST Depression (OldPeak)
- Slope of Peak Exercise (1 = Upsloping, 2 = Flat, 3 = Downsloping)
- Number of Major Vessels (0-3)
- Thalassemia (3 = Normal, 6 = Fixed Defect, 7 = Reversible Defect)

### Diabetes Prediction
- Times Pregnant
- Plasma Glucose Level
- Diastolic Blood Pressure
- Tricep Skin Fold Thickness
- Serum Insulin Level
- Body Mass Index (BMI)
- Diabetes Pedigree Function
- Age

## Algorithm

The system uses Naive Bayes classification algorithms trained on:
- Heart Disease Dataset: Contains 13 features for cardiovascular risk assessment
- Diabetes Dataset: Contains 8 features for diabetes risk evaluation

The models provide probability scores that are visualized through interactive bar charts.

## Dataset Information

- **HeartDisease.csv**: Medical data for heart disease prediction
- **DiabetesData.csv**: Health indicators for diabetes prediction

Both datasets are preprocessed and ready for machine learning model training.

## Contributing

Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## Code of Conduct

This project adheres to a code of conduct. Please read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) for details.

## License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

## Disclaimer

This application is for educational and research purposes only. The predictions should not be used as a substitute for professional medical advice, diagnosis, or treatment. Always consult with qualified healthcare providers for medical decisions.

## Author

Niraj Kumar

## Support

For support and questions, please open an issue in the GitHub repository.
# Airline-management-Case-study

Airline Management System Case Study developed in COBOL on IBM AS400 (IBM i), demonstrating passenger booking, flight scheduling, ticket management, and customer record processing using DB2/400 and CL programming concepts.

# Airline Management System - COBOL AS400 Case Study

## Overview

This project is a case study of an Airline Management System developed using COBOL on the IBM AS400 (IBM i) platform. The application simulates core airline operations such as flight management, passenger reservations, ticket booking, and reporting while demonstrating enterprise application development practices on AS400 systems.

The project showcases COBOL programming, DB2/400 database operations, file handling, batch processing, and CL program execution commonly used in enterprise business applications.

## Features

### Flight Management

* Add new flight records
* Update flight schedules
* View flight information
* Search flights by route and flight number

### Passenger Management

* Register passenger details
* Update passenger records
* Maintain passenger database

### Reservation & Ticket Booking

* Book flight tickets
* Cancel reservations
* Check seat availability
* Generate booking confirmations

### Reporting

* Passenger manifest reports
* Flight occupancy reports
* Daily booking summaries
* Cancellation reports

## Technology Stack

* Language: COBOL
* Platform: IBM AS400 (IBM i)
* Database: DB2/400
* Job Management: CL Programs
* Data Storage: Physical Files and Logical Files
* Utilities: Query/400, DB2 Utilities
* Development Environment: SEU, PDM, STRSQL

## Project Structure

AIRLINE-MANAGEMENT/

├── COBOL/

│   ├── FLIGHTM.cbl

│   ├── PASSENGERM.cbl

│   ├── BOOKINGM.cbl

│   └── REPORTM.cbl

│

├── CL/

│   ├── COMPILE.clp

│   ├── BUILD.clp

│   └── RUNAPP.clp

│

├── DDS/

│   ├── FLIGHTPF.dspf

│   ├── PASSENGERPF.dspf

│   └── BOOKINGPF.dspf

│

├── DATABASE/

│   ├── FLIGHT

│   ├── PASSENGER

│   └── BOOKING

│

└── README.md

## Business Scenario

An airline company requires a centralized system to manage:

* Flight schedules
* Passenger information
* Ticket reservations
* Seat allocation
* Operational reports

The system stores and processes airline data using DB2/400 database files and COBOL business logic, providing efficient record management and transaction processing.

## Learning Objectives

This case study demonstrates:

* COBOL programming on IBM i
* DB2/400 database operations
* Physical and Logical File management
* CL program execution
* Modular application design
* Enterprise business application development concepts

## Sample Workflow

1. Create flight records.
2. Register passengers.
3. Book flight reservations.
4. Update seat inventory.
5. Generate operational reports.
6. Execute batch processes through CL programs.

## Future Enhancements

* Interactive display file screens
* Embedded SQL integration
* Web-enabled interfaces
* Real-time seat allocation
* Automated report scheduling
* API integration with external systems

## Author

Developed as a COBOL AS400 Case Study for learning and demonstrating enterprise airline management processes on IBM AS400 (IBM i) systems.

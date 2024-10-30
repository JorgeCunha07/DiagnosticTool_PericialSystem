package org.dei;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Main class for the Diagnostic Tool application.
 * This class is responsible for bootstrapping the Spring Boot application.
 */
@SpringBootApplication
public class DiagnosticToolApplication {

    /**
     * The main method which serves as the entry point for the Spring Boot application.
     *
     * @param args command-line arguments passed to the application
     */
    public static void main(String[] args) {
        SpringApplication.run(DiagnosticToolApplication.class, args);
    }
}
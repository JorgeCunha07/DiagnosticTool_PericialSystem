package org.dei.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;
import org.springframework.beans.factory.annotation.Value;

/**
 * Web configuration class to enable CORS settings for the application.
 */
@Configuration
@EnableWebMvc
public class WebConfig extends WebMvcConfigurerAdapter {

    /**
     * The URL of the frontend API, injected from the application properties.
     */
    @Value("${frontend.api.url}")
    private String apiUrl;

    /**
     * Configures CORS mappings for the application.
     *
     * @param registry the CORS registry to add mappings to
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins(apiUrl)
                .allowedHeaders("*")
                .allowedMethods("GET", "POST")
                .allowCredentials(true);
    }
}
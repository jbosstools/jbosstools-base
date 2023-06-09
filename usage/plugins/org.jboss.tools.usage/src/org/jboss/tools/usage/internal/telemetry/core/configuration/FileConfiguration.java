/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.telemetry.core.configuration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;

public class FileConfiguration extends AbstractConfiguration {

    private final UsagePluginLogger logger = JBossToolsUsageActivator.getDefault().getLogger();

    protected final Path path;

    public FileConfiguration(Path path) {
        this.path = path;
    }

    public boolean exists() {
    	return  Files.exists(path);
    }

    public boolean create() {
    	try {
			Files.createFile(path);
			return true;
		} catch (IOException e) {
			logger.error(e, false);
			return false;
		}
    }

    @Override
    protected Properties loadProperties() {
        Properties properties = new Properties();
        try (InputStream in = createInputStream(path)) {
            if (in != null) {
                properties.load(in);
            }
        } catch (IOException e) {
            logger.warn("Could not load properties file " + (path == null? "" : path.toAbsolutePath()), null);
        }
        return properties;
    }

    protected InputStream createInputStream(Path path) throws IOException {
        if (path == null) {
            return null;
        }
        File file = path.toFile();
        if (!file.exists()) {
            return null;
        }
        return new FileInputStream(file);
    }
}

/*******************************************************************************
 * Copyright (c) 2022 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package com.redhat.devtools.intellij.telemetry.core.service.segment;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Objects;
import java.util.stream.Stream;

import org.eclipse.core.runtime.ILog;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.redhat.devtools.intellij.telemetry.core.util.Directories;
import com.redhat.devtools.intellij.telemetry.core.util.FileUtils;

/**
 * Persistency for {@link IdentifyTraits}.
 */
public class IdentifyTraitsPersistence {


    public static final IdentifyTraitsPersistence INSTANCE = new IdentifyTraitsPersistence();
    private static final Path FILE = Directories.RED_HAT.resolve("segment-identify-traits.json");
    private final ILog logger = JBossToolsUsageActivator.getDefault().getLog();
    private final JsonMapper mapper = new JsonMapper();
    
    private IdentifyTraits identifyTraits = null;

    protected IdentifyTraitsPersistence() {}

    public synchronized IdentifyTraits get() {
        if (identifyTraits == null) {
            this.identifyTraits = deserialize(load(FILE));
        }
        return identifyTraits;
    }

    public synchronized void set(IdentifyTraits identifyTraits) {
        if (Objects.equals(identifyTraits, this.identifyTraits)) {
            return;
        }
        this.identifyTraits = identifyTraits;
        String string = null;
        if (identifyTraits != null) {
            string = serialize(identifyTraits);
        }
        save(string, FILE);
    }

    private String serialize(IdentifyTraits identifyTraits) {
        if (identifyTraits == null) {
            return null;
        }
        try {
			return mapper.writeValueAsString(identifyTraits);
		} catch (JsonProcessingException e) {
			logger.error("Could not deserialize identity traits " + identifyTraits, e);
			return null;
		}
    }

    private IdentifyTraits deserialize(String identity) {
        if (identity == null) {
            return null;
        }
        try {
			return mapper.readValue(identity, new TypeReference<IdentifyTraits>(){});
		} catch (JsonProcessingException e) {
			logger.error("Could not deserialize identity traits " + identity, e);
			return null;
		}
    }

    private String load(Path file) {
        String event = null;
        try(Stream<String> lines = getLines(file)) {
            event = lines
                    .findAny()
                    .map(String::trim)
                    .orElse(null);
        } catch (IOException e) {
            logger.warn("Could not read identity file at " + file.toAbsolutePath(), e);
        }
        return event;
    }

    /* for testing purposes */
    protected Stream<String> getLines(Path file) throws IOException {
        return Files.lines(file);
    }

    private void save(String event, Path file) {
        try {
            createFileAndParent(file);
            writeFile(event, file);
        } catch (IOException e) {
            logger.warn("Could not write identity to file at " + FILE.toAbsolutePath(), e);
        }
    }

    /* for testing purposes */
    protected void createFileAndParent(Path file) throws IOException {
        FileUtils.createFileAndParent(file);
    }

    /* for testing purposes */
    protected void writeFile(String event, Path file) throws IOException {
        FileUtils.write(event, file);
    }
}

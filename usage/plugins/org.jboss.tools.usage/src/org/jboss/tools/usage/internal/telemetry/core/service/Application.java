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
package org.jboss.tools.usage.internal.telemetry.core.service;

import java.util.AbstractMap;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

public class Application {

    private final String name;
    private final String version;
    private final Map<String, String> properties = new HashMap<>();

    Application(String name, String version) {
        this.name = name;
        this.version = version;
    }

    public String getName() {
        return name;
    }

    public String getVersion() {
        return version;
    }

    public Application property(String key, String value) {
        this.properties.put(key, value);
        return this;
    }

    public Collection<AbstractMap.SimpleEntry<String, Object>> getProperties() {
        return properties.entrySet().stream()
                .map(entry -> new AbstractMap.SimpleEntry<String, Object>(entry.getKey(), entry.getValue()))
                .collect(Collectors.toList());
    }

	@Override
	public int hashCode() {
		return Objects.hash(name, properties, version);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Application other = (Application) obj;
		return Objects.equals(name, other.name) 
				&& Objects.equals(properties, other.properties)
				&& Objects.equals(version, other.version);
	}


}

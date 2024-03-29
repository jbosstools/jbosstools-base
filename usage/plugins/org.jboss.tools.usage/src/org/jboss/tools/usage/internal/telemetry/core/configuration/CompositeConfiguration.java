/*******************************************************************************
 * Copyright (c) 2021 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * copied from https://github.com/redhat-developer/intellij-redhat-telemetry
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.telemetry.core.configuration;

import java.util.List;
import java.util.Objects;

public abstract class CompositeConfiguration implements IConfiguration {

    @Override
    public String get(final String key) {
        List<IConfiguration> configurations = getConfigurations();
        if (configurations == null
                || configurations.isEmpty()) {
            return null;
        }
        return configurations.stream()
                .map(configuration -> configuration.get(key))
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }

    protected abstract List<IConfiguration> getConfigurations();

}

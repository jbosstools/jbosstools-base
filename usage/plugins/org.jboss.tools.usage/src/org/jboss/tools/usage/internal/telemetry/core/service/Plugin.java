/*******************************************************************************
 * Copyright (c) 2022 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v2.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v20.html
 *
 * Contributors:
 * copied from https://github.com/redhat-developer/intellij-redhat-telemetry 
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.internal.telemetry.core.service;

public class Plugin extends Application {

    public static final class Factory {

        public Plugin create(String name, String version) {
            return new Plugin(name, version);
        }

    }

    Plugin(String name, String version) {
        super(name, version);
    }

    @Override
    public Plugin property(String key, String value) {
        super.property(key, value);
        return this;
    }

}

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

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.environment.eclipse.IEclipsePlatform;

public class IDE extends Application {

    public static final String PROP_JAVA_VERSION = "java_version";

    public static final class Factory {
        public IDE create() {
        	IEclipsePlatform eclipsePlatform = 
        			JBossToolsUsageActivator.getDefault().getJBossToolsEclipseEnvironment().getEclipsePlatform();
        	return create(eclipsePlatform);
        }

    	public IDE create(IEclipsePlatform eclipsePlatform) {
            return new IDE(
                    eclipsePlatform.getApplicationName(),
                    eclipsePlatform.getApplicationVersion());
        }
    }

    IDE(String applicationName, String applicationVersion) {
        super(applicationName, applicationVersion);
    }

    public IDE setJavaVersion() {
        return setJavaVersion(System.getProperty("java.version"));
    }

    public IDE setJavaVersion(String version) {
        property(PROP_JAVA_VERSION, version);
        return this;
    }

    @Override
    public IDE property(String key, String value) {
        super.property(key, value);
        return this;
    }

}

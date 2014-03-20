/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.test.fakes;

import java.io.IOException;
import java.util.Map;

import org.eclipse.core.runtime.Plugin;
import org.jboss.tools.usage.internal.http.IPropertiesProvider;
import org.jboss.tools.usage.internal.preferences.GlobalUsageSettings;

/**
 * @author Alexey Kazakov
 */
public class GlobalUsageSettingsForEventRegisterTest extends GlobalUsageSettings {

	private Map<Object, Object> properties;

	public GlobalUsageSettingsForEventRegisterTest(Plugin plugin, Map<Object, Object> properties) {
		super(plugin);
		this.properties = properties;
	}

	@Override
	protected IPropertiesProvider createRemoteMap(String url, Plugin plugin) {
		return new IPropertiesProvider() {
			@Override
			public Map<Object, Object> getMap() throws IOException {
				return properties;
			}
		};
	}

	@Override
	public boolean isReportingEnabled() {
		return isAllInstancesReportingEnabled();
	}
}
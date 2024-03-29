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
package org.jboss.tools.usage.telemetry;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.jboss.tools.usage.event.UsageEvent;
import org.jboss.tools.usage.event.UsageEventType;
import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.environment.eclipse.IJBossToolsEclipseEnvironment;
import org.jboss.tools.usage.internal.event.EventSender;
import org.jboss.tools.usage.internal.event.RequestType;
import org.jboss.tools.usage.internal.telemetry.core.configuration.TelemetryConfiguration;
import org.jboss.tools.usage.internal.telemetry.core.configuration.TelemetryConfiguration.Mode;
import org.jboss.tools.usage.internal.telemetry.core.service.Plugin;
import org.jboss.tools.usage.internal.telemetry.core.service.TelemetryMessageBuilder;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;

public class TelemetrySender implements EventSender {

	private UsagePluginLogger logger = JBossToolsUsageActivator.getDefault().getLogger();
	private Map<Plugin, TelemetryMessageBuilder> telemetryByPlugin = new ConcurrentHashMap<>();

	public TelemetrySender() {
		configureMode(TelemetryConfiguration.getInstance());
	}

	@Override
	public boolean sendRequest(IJBossToolsEclipseEnvironment environment, 
			String title,
			UsageEvent event, 
			RequestType requestType) {
		TelemetryMessageBuilder telemetry = getOrCreateTelemetry(event);
		UsageEventType eventType = event.getType();
		telemetry
			.action(eventType.getActionName())
			.property("requestType", String.valueOf(requestType))
			.property("category", eventType.getCategoryName())
			.property(eventType.getLabelDescription(), event.getLabel())
			.property(eventType.getValueDescription(), String.valueOf(event.getValue()))
			.send();
		return true;
	}

	/**
	 * Return an existing telemetry instance or a creates a new one if it doesn't exist yet.
	 * @param event
	 * @return
	 */
	private TelemetryMessageBuilder getOrCreateTelemetry(UsageEvent event) {
		String name = event.getType().getComponentName();
		String version = event.getType().getComponentVersion();
		Plugin plugin = new Plugin.Factory().create(name, version);
		return telemetryByPlugin.computeIfAbsent(plugin, pluginIdentifier -> createTelemetry(plugin));
	}

	private TelemetryMessageBuilder createTelemetry(Plugin plugin) {
		logger.debug("creating new telemetry instance for " + plugin.getName() + " " + plugin.getVersion());
		return new TelemetryMessageBuilder(plugin);
	}

	private void configureMode(TelemetryConfiguration configuration) {
		TelemetryConfiguration.Mode mode = configuration.getMode();
		if (mode == null
				|| !mode.isConfigured()) {
			// preferences set to accept usage reporting but mode not configured yet. 
			// Should not overwrite existing telemetry configuration. 
			// Could have been configured manually (ex. set to DEBUG).
			configuration.setMode(TelemetryConfiguration.Mode.NORMAL);
		}
	}
}

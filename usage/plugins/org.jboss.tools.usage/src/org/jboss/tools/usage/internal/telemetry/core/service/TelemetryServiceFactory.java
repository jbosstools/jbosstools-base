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
package org.jboss.tools.usage.internal.telemetry.core.service;

import org.jboss.tools.usage.internal.telemetry.core.IMessageBroker;
import org.jboss.tools.usage.internal.telemetry.core.configuration.TelemetryConfiguration;
import org.jboss.tools.usage.internal.telemetry.core.service.segment.SegmentBroker;
import org.jboss.tools.usage.internal.telemetry.core.service.segment.SegmentConfiguration;

public class TelemetryServiceFactory {

	private final IDE ide = new IDE.Factory()
			.create()
			.setJavaVersion();

	public TelemetryService create(Plugin plugin) {
		Environment environment = new Environment.Builder()
				.ide(ide)
				.plugin(plugin)
				.build();
		TelemetryConfiguration configuration = TelemetryConfiguration.getInstance();
		IMessageBroker broker = createSegmentBroker(configuration.isDebug(), environment);
		return new TelemetryService(configuration, broker);
	}

	private IMessageBroker createSegmentBroker(boolean isDebug, Environment environment) {
		SegmentConfiguration brokerConfiguration = new SegmentConfiguration();
		return new SegmentBroker(isDebug, UserId.INSTANCE.get(), environment, brokerConfiguration);
	}

}

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
package com.redhat.devtools.intellij.telemetry.core.service;

import com.redhat.devtools.intellij.telemetry.core.IMessageBroker;
import com.redhat.devtools.intellij.telemetry.core.configuration.TelemetryConfiguration;
import com.redhat.devtools.intellij.telemetry.core.service.segment.SegmentBroker;
import com.redhat.devtools.intellij.telemetry.core.service.segment.SegmentConfiguration;

public class TelemetryServiceFactory {

    private final Environment.Builder builder = new Environment.Builder()
            .ide(new IDE.Factory().create().setJavaVersion());

    public TelemetryService create() {
        Environment environment = builder.plugin().build();
        TelemetryConfiguration configuration = TelemetryConfiguration.getInstance();
        IMessageBroker broker = createSegmentBroker(configuration.isDebug(), environment);
        return new TelemetryService(configuration, broker);
    }

    private IMessageBroker createSegmentBroker(boolean isDebug,  Environment environment) {
        SegmentConfiguration brokerConfiguration = new SegmentConfiguration();
        return new SegmentBroker(
                isDebug,
                UserId.INSTANCE.get(),
                environment,
                brokerConfiguration);
    }

}

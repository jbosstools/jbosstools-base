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
import org.jboss.tools.usage.internal.telemetry.core.ITelemetryService;
import org.jboss.tools.usage.internal.telemetry.core.configuration.TelemetryConfiguration;
import org.jboss.tools.usage.internal.telemetry.core.service.TelemetryEvent.Type;
import org.jboss.tools.usage.internal.telemetry.core.util.CircularBuffer;

public class TelemetryService implements ITelemetryService {

    private static final int BUFFER_SIZE = 35;

    private final TelemetryConfiguration configuration;
    protected final IMessageBroker broker;
    private final CircularBuffer<TelemetryEvent> onHold = new CircularBuffer<>(BUFFER_SIZE);

    TelemetryService(final TelemetryConfiguration configuration,
                     final IMessageBroker broker) {
        this.configuration = configuration;
        this.broker = broker;
    }

    @Override
    public void send(TelemetryEvent event) {
        sendUserInfo();
        doSend(event);
    }

    private void sendUserInfo() {
        doSend(new TelemetryEvent(
                Type.USER,
                "Anonymous ID: " + UserId.INSTANCE.get()));
    }

    private void doSend(TelemetryEvent event) {
        if (isEnabled()) {
            flushOnHold();
            broker.send(event);
        } else if (!isConfigured()) {
            onHold.offer(event);
        }
    }

    private boolean isEnabled() {
        return configuration != null
                && configuration.isEnabled();
    }

    private boolean isConfigured() {
        return configuration != null
                && configuration.isConfigured();
    }

    private void flushOnHold() {
        onHold.pollAll().forEach(this::send);
    }

    public void dispose() {
        flushOnHold();
        onHold.clear();
        broker.dispose();
    }
}

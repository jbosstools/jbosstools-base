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
package org.jboss.tools.usage.internal.telemetry.core.service.segment;

import static java.lang.Thread.MIN_PRIORITY;

import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.jboss.tools.usage.internal.telemetry.core.IMessageBroker;
import org.jboss.tools.usage.internal.telemetry.core.service.Application;
import org.jboss.tools.usage.internal.telemetry.core.service.Environment;
import org.jboss.tools.usage.internal.telemetry.core.service.TelemetryEvent;
import org.jboss.tools.usage.internal.telemetry.core.util.Lazy;
import org.jboss.tools.usage.internal.telemetry.core.util.MapBuilder;
import org.jboss.tools.usage.tracker.internal.UsagePluginLogger;

import com.segment.analytics.Analytics;
import com.segment.analytics.messages.IdentifyMessage;
import com.segment.analytics.messages.MessageBuilder;
import com.segment.analytics.messages.PageMessage;
import com.segment.analytics.messages.TrackMessage;

public class SegmentBroker implements IMessageBroker {


    public static final String PROP_NAME = "name";
    public static final String PROP_VERSION = "version";
    public static final String PROP_APP = "app";
    public static final String PROP_IP = "ip";
    public static final String PROP_COUNTRY = "country";
    public static final String PROP_LOCALE = "locale";
    public static final String PROP_LOCATION = "location";
    public static final String PROP_OS = "os";
    public static final String PROP_OS_NAME = "os_name";
    public static final String PROP_OS_DISTRIBUTION = "os_distribution";
    public static final String PROP_OS_VERSION = "os_version";
    public static final String PROP_TIMEZONE = "timezone";

    public static final String VALUE_NULL_IP = "0.0.0.0"; // fixed, faked ip addr

    public static final String PROP_EXTENSION_NAME = "extension_name";
    public static final String PROP_EXTENSION_VERSION = "extension_version";
    public static final String PROP_APP_NAME = "app_name";
    public static final String PROP_APP_VERSION = "app_version";

    private final UsagePluginLogger logger = JBossToolsUsageActivator.getDefault().getLogger();

    enum SegmentType {
        IDENTIFY {
            public MessageBuilder<?, ?> toMessage(TelemetryEvent event, Map<String, Object> context, SegmentBroker broker) {
                return broker.toMessage(IdentifyMessage.builder(), event, context);
            }
        },
        TRACK {
            public MessageBuilder<?, ?> toMessage(TelemetryEvent event, Map<String, Object> context, SegmentBroker broker) {
                return broker.toMessage(TrackMessage.builder(event.getName()), event, context);
            }
        },
        PAGE {
            public MessageBuilder<?, ?> toMessage(TelemetryEvent event, Map<String, Object> context, SegmentBroker broker) {
                return broker.toMessage(PageMessage.builder(event.getName()), event, context);
            }

        };

        public abstract MessageBuilder<?, ?> toMessage(TelemetryEvent event, Map<String, Object> context, SegmentBroker broker);

        public static SegmentType valueOf(TelemetryEvent.Type eventType) {
            switch (eventType) {
                case USER:
                    return IDENTIFY;
                case ACTION:
                case STARTUP:
                case SHUTDOWN:
                default:
                    return TRACK;
            }
        }
    }

    private final String userId;
    private final IdentifyTraitsPersistence identifyTraitsPersistence;
    private final Environment environment;
    private Lazy<Analytics> analytics;

    public SegmentBroker(boolean isDebug, String userId, Environment environment, ISegmentConfiguration configuration) {
        this(isDebug, userId, IdentifyTraitsPersistence.INSTANCE, environment, configuration, new AnalyticsFactory());
    }

    public SegmentBroker(boolean isDebug, String userId, IdentifyTraitsPersistence identifyTraitsPersistence, Environment environment, ISegmentConfiguration configuration, Function<String, Analytics> analyticsFactory) {
        this.userId = userId;
        this.identifyTraitsPersistence = identifyTraitsPersistence;
        this.environment = environment;
        this.analytics = new Lazy<>(() -> analyticsFactory.apply(getWriteKey(isDebug, configuration)));
    }

    @Override
    public void send(TelemetryEvent event) {
        try {
            if (analytics.get() == null) {
                logger.warn("Could not send " + event.getType() + " event '" + event.getName() + "': no analytics instance present.", null);
                return;
            }
            Map<String, Object> context = createContext(environment);
            SegmentType segmentType = SegmentType.valueOf(event.getType());
            MessageBuilder<?, ?> builder = segmentType.toMessage(event, context, this);
            if (builder == null) {
                logger.debug("No message to be sent.");
            } else {
                logger.debug("Sending message " + builder.type() + " to segment.");
                analytics.get().enqueue(builder);
            }
        } catch (IllegalArgumentException e) {
            logger.warn("Could not send " + event.getName() + " event: unknown type '" + event.getType() + "'.", null);
        }
    }

    private MessageBuilder<?, ?> toMessage(IdentifyMessage.Builder builder, TelemetryEvent event, Map<String, Object> context) {
        IdentifyTraits identifyTraits = new IdentifyTraits(
                environment.getLocale(),
                environment.getTimezone(),
                environment.getPlatform().getName(),
                environment.getPlatform().getVersion(),
                environment.getPlatform().getDistribution());
        if (!haveChanged(identifyTraits, identifyTraitsPersistence)) {
            logger.debug("Skipping identify message: already sent." + identifyTraits);
            return null;
        }
        return builder
                .userId(userId)
                .traits(addIdentifyTraits(identifyTraits, event.getProperties()))
                .context(context);
    }

    /**
     * Saves the given identify traits to persistence if persistence exists.
     *
     * @param identifyTraits the traits to save
     * @return true if saving occurred or no persistence was present.
     */
    private synchronized boolean haveChanged(IdentifyTraits identifyTraits, IdentifyTraitsPersistence persistence) {
        if (identifyTraitsPersistence != null) {
            if (identifyTraits.equals(persistence.get())) {
                return false;
            } else {
                persistence.set(identifyTraits);
            }
        }
        return true;
    }

    private Map<String, ?> addIdentifyTraits(final IdentifyTraits identifyTraits, final Map<String, String> properties) {
        putIfNotNull(PROP_LOCALE, identifyTraits.getLocale(), properties);
        putIfNotNull(PROP_TIMEZONE, identifyTraits.getTimezone(), properties);
        putIfNotNull(PROP_OS_NAME, identifyTraits.getOsName(), properties);
        putIfNotNull(PROP_OS_DISTRIBUTION, identifyTraits.getOsDistribution(), properties);
        putIfNotNull(PROP_OS_VERSION, identifyTraits.getOsVersion(), properties);
        return properties;
    }

    private MessageBuilder<?, ?> toMessage(TrackMessage.Builder builder, TelemetryEvent event, Map<String, Object> context) {
        return builder
                .userId(userId)
                .properties(addTrackProperties(event.getProperties()))
                .context(context);
    }

    private Map<String, ?> addTrackProperties(final Map<String, String> properties) {
        Application application = environment.getIde();
        putIfNotNull(PROP_APP_NAME, application.getName(), properties);
        putIfNotNull(PROP_APP_VERSION, application.getVersion(), properties);
        application.getProperties().forEach(
                appProperty -> putIfNotNull(appProperty.getKey(), String.valueOf(appProperty.getValue()), properties));
        putIfNotNull(PROP_EXTENSION_NAME, environment.getPlugin().getName(), properties);
        putIfNotNull(PROP_EXTENSION_VERSION, environment.getPlugin().getVersion(), properties);
        return properties;
    }

    private MessageBuilder<?, ?> toMessage(PageMessage.Builder builder, TelemetryEvent event, Map<String, Object> context) {
        return builder
                .userId(userId)
                .properties(event.getProperties())
                .context(context);
    }

    private void putIfNotNull(String key, String value, Map<String, String> properties) {
        if (key == null
                || value == null
                || properties == null) {
            return;
        }
        properties.put(key, value);
    }

    private Map<String, Object> createContext(Environment environment) {
        return new MapBuilder()
                .mapPair(PROP_APP)
                    .pair(PROP_NAME, environment.getIde().getName())
                    .pair(PROP_VERSION, environment.getIde().getVersion())
                    .pairs(environment.getIde().getProperties())
                    .build()
                .pair(PROP_IP, VALUE_NULL_IP)
                .pair(PROP_LOCALE, environment.getLocale())
                .mapPair(PROP_LOCATION)
                    .pair(PROP_COUNTRY, environment.getCountry())
                    .build()
                .mapPair(PROP_OS)
                    .pair(PROP_NAME, environment.getPlatform().getName())
                    .pair(PROP_VERSION, environment.getPlatform().getVersion())
                    .build()
                .pair(PROP_TIMEZONE, environment.getTimezone())
                .build();
    }

    public void dispose() {
        analytics.get().flush();
        analytics.get().shutdown();
    }


    private String getWriteKey(boolean isDebug, ISegmentConfiguration configuration) {
        if (isDebug) {
            return configuration.getDebugWriteKey();
        } else {
            return configuration.getNormalWriteKey();
        }
    }

    private static class AnalyticsFactory implements Function<String, Analytics> {

        private static final int FLUSH_INTERVAL = 10000;
        private static final int FLUSH_QUEUE_SIZE = 10;

        private final UsagePluginLogger logger = JBossToolsUsageActivator.getDefault().getLogger();

        @Override
        public Analytics apply(String writeKey) {
            if (writeKey == null) {
                logger.warn("Could not create Segment Analytics instance, missing writeKey.", null);
                return null;
            }
            logger.debug("Creating Segment Analytics instance using " + writeKey + " writeKey.");
            return Analytics.builder(writeKey)
                    .flushQueueSize(FLUSH_QUEUE_SIZE)
                    .flushInterval(FLUSH_INTERVAL, TimeUnit.MILLISECONDS)
                    .threadFactory(new ThreadFactory() {

                    @Override
                    public Thread newThread(Runnable runnable) {
                        return new Thread(new Runnable() {

                            @Override
                            public void run() {
                                ClassLoader classLoader = 
                                        JBossToolsUsageActivator.getDefault().getBundleClassLoader();
                                Thread currentThread = Thread.currentThread();
                                runWithContextClassLoader(classLoader, currentThread, runnable);
                            }

                            private void runWithContextClassLoader(ClassLoader classLoader, Thread currenThread,
                                    Runnable runnable) {
                                ClassLoader backup = currenThread.getContextClassLoader();
                                currenThread.setContextClassLoader(classLoader);
                                currenThread.setPriority(MIN_PRIORITY);
                                try {
                                    runnable.run();
                                } finally {
                                    currenThread.setContextClassLoader(backup);
                                }
                            }

                        },  "Telemetry");
                    }
                })
                .build();
            }
    }
    
}

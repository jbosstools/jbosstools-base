package org.jboss.tools.usage.http;

import java.util.Dictionary;

import org.jboss.tools.usage.internal.JBossToolsUsageActivator;
import org.osgi.service.cm.ConfigurationException;
import org.osgi.service.cm.ManagedService;
import org.osgi.service.component.ComponentContext;

public class HttpRemotePropertiesProviderFactory implements IPropertiesProviderFactory, ManagedService {

	public static final String USAGE_REPORTING_ENABLED_KEY = "usage_reporting_enabled";

	public static final String REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY = USAGE_REPORTING_ENABLED_KEY + "="; //$NON-NLS-1$

	/** the delimiter that delimits the key/value-pairs */
	private static final char VALUE_DELIMITER = '\n';

	private String url;

	public IPropertiesProvider create() {

		return new HttpRemotePropertiesProvider(
				url
				, VALUE_DELIMITER
				, JBossToolsUsageActivator.getDefault()
				, REMOTEPROPS_USAGE_REPORTING_ENABLED_KEY);
	}

	protected void activate(ComponentContext componentContext) {
		System.err.println("activated");
		Dictionary properties = componentContext.getProperties();
		url = (String) properties.get("url");
	}

	public void updated(Dictionary properties) throws ConfigurationException {
		System.err.println("modified");
		url = (String) properties.get("url");
	}

}

package org.jboss.tools.tests.tests;

import java.util.HashSet;
import java.util.Set;

public interface IDEPropertiesConstants {

	@SuppressWarnings("serial")
	static final Set<String> knownProperties = new HashSet<String>() {
		{
			add("jboss.discovery.earlyaccess.site.url");
			add("examples.categories.url");
			add("jboss.discovery.site.integration-stack.url");
			add("jboss.discovery.earlyaccess.list.url");
			add("jboss.discovery.site.url");
			add("jboss.discovery.directory.url");
			add("portal.examples.url");
			add("community.examples.url");
			add("maven.examples.url");
			add("featured.examples.url");
			add("buzz.feed.url");
		}
	};
	
	// The default url for ide.properties
	// TODO: maybe allow this to be overridden for testing other urls ?
	final static String PROPERTIES_LOCATION = "http://download.jboss.org/jbosstools/configuration/ide-config.properties";
	@SuppressWarnings("serial")
	static final Set<String> knownTargets = new HashSet<String>() {
		{
			add("devstudio");
			add("jbosstools");
		}
	};
	

	@SuppressWarnings("serial")
	static final Set<String> knownVersionQualifiers = new HashSet<String>() {
		{
			add("Alpha1");
			add("Alpha2");
			add("Beta1");
			add("Beta2");
			add("Beta3");
			add("CR1");
			add("Final");
			add("GA");
			
		}
	};
	
}

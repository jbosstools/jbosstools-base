/*******************************************************************************
 * Copyright (c) 2014 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.foundation.core.properties.internal;

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.jboss.tools.foundation.core.FoundationCorePlugin;
import org.jboss.tools.foundation.core.ecf.URLTransportUtility;
import org.jboss.tools.foundation.core.properties.IPropertiesProvider;

/**
 * Provides properties depending on the context (project/product) it's invoked in and the version of the context. Context and version are defined by the current {@link VersionPropertiesProvider} instance.
 * <p>
 * Key/Value pairs are expected to be stored using the <code>key|context|version=value</code> format.
 * </p>
 * <p>
 * If an exact matching version is not found for the requested key, the properties provider will try to fall back to parent versions.
 * </p>
 * <p>for instance, if the key <code>foo</code> is requested in a {@link VersionPropertiesProvider} provided by JBDS 8.0.0.Beta2-123456-65432 then : </p>
 * <ul>
 * 	<li>It will return <code>bar</code> if <code>foo|devstudio|8.0.0.Beta2=bar</code> is set.</li>
 *	<li>If no matching key/version is defined, then <code>foo|devstudio|8.0.0</code> would be queried,</li>
 *	<li>then <code>foo|devstudio|8.0</code>,</li>
 *	<li>then <code>foo|devstudio|8</code>,</li>
 *	<li>then <code>foo|devstudio</code>,</li>
 *	<li>then <code>foo</code>.</li>
 * </ul>
 * <p>
 * Properties are retrieved from a URI (<a href="http://download.jboss.org/jbosstools/configuration/ide-config.properties">http://download.jboss.org/jbosstools/configuration/ide-config.properties</a> by default).
 * This URI can be overridden using the <code>org.jboss.tools.foundation.core.config.properties.url</code> VM argument. For instance, to test a local properties file, the following argument needs to be added to the JVM
 * <pre>-Dorg.jboss.tools.foundation.core.config.properties.url=file://path/to/resource.properties</pre>
 * </p>
 * @author Fred Bricon
 * @since 1.1.0
 */
public class VersionPropertiesProvider implements IPropertiesProvider, IExecutableExtension {

  public static String VERSION_PROPERTIES_URI_KEY = FoundationCorePlugin.PLUGIN_ID+ ".config.properties.url";

  private static String DEFAULT_PROPERTIES_FILE    = "ide-config.properties";

  private static String DEFAULT_PROPERTIES_URI     = "http://download.jboss.org/jbosstools/configuration/"
      + DEFAULT_PROPERTIES_FILE;

  private Properties properties;

  private String currentVersion = null;

  private URI propertiesURI;

  private String context;

  private String id;

  public VersionPropertiesProvider() {
    this((String)null, null, null);
  }

  //For testing purposes
  VersionPropertiesProvider(String propertiesURI, String projectContext, String version) {
    super();
    initPropertiesUri(propertiesURI);
    context = projectContext;
    id = context + ".properties.provider";
    currentVersion = version;
  }

  //For testing purposes
  VersionPropertiesProvider(Properties properties, String projectContext, String version) {
    this((String)null, projectContext, version);
    this.properties = properties;
  }

  protected String getCurrentVersion() {
    if (currentVersion == null) {
      currentVersion = VersionExtractor.getVersion(getVersionBundleName(), getClass().getClassLoader());
    }
    return currentVersion;
  }

  protected String getVersionBundleName() {
    return "org.jboss.tools.foundation.core.properties.internal.currentversion";
  }

  protected String getContext() {
    return context;
  }

  public String getId() {
    return id;
  }

  protected void initPropertiesUri(String propertiesURI) {
    try {
      if (propertiesURI == null) {
        this.propertiesURI = new URI(System.getProperty(VERSION_PROPERTIES_URI_KEY, DEFAULT_PROPERTIES_URI));
      } else {
        this.propertiesURI = new URI(propertiesURI);
      }
    } catch (URISyntaxException e) {
      FoundationCorePlugin.pluginLog().logError(
          "Invalid URI format (" + propertiesURI + ") for "
              + VERSION_PROPERTIES_URI_KEY + ". Falling back on "
              + DEFAULT_PROPERTIES_URI, e);
      this.propertiesURI = URI.create(DEFAULT_PROPERTIES_URI);
    }
  }

  @Override
  public String getValue(String key, String defaultValue) {
    if (properties == null) {
      try {
        properties = loadProperties(propertiesURI, new NullProgressMonitor());
      } catch (CoreException e) {
        FoundationCorePlugin.pluginLog().logError(
            "Unable to load properties from " + propertiesURI
            + ". Falling back on embedded properties", e);
      }
      if (properties == null || properties.isEmpty()) {
        properties = loadDefaultProperties();
      }

      String resolvedPropsAsString = dump(properties);
      System.setProperty("org.jboss.tools.resolved.remote.properties",
          resolvedPropsAsString);
    }

    // properties can't be null at this point, unless we really really borked
    // the build
    String value = lookupValue(key, getContext(), getCurrentVersion(),
        properties);

    return value == null ? defaultValue : value;
  }

  String dump(Properties props) {
    if (props == null || props.isEmpty()) {
      return "!!Empty properties!!";
    }
    StringBuilder output = new StringBuilder();
    String crlf = System.getProperty("line.separator");
    Set<String> baseKeys = new HashSet<String>();
    for (Object key : props.keySet()) {
      baseKeys.add(key.toString().split("\\|")[0]);
    }

    SimpleHierarchicalVersion version = new SimpleHierarchicalVersion(
        getCurrentVersion());
    for (String key : baseKeys) {
      String matchingKey = lookupKey(key, getContext(), version, props);
      if (matchingKey != null) {
        output.append(matchingKey).append("=")
            .append(props.getProperty(matchingKey)).append(crlf);
      }
    }
    return output.toString();
  }

  protected Properties loadDefaultProperties() {
    try {
      return getProperties(VersionPropertiesProvider.class.getResourceAsStream(
          DEFAULT_PROPERTIES_FILE));
    } catch (Exception e) {
      // Shouldn't happen unless Maven wasn't called to download the remote
      // properties file in the source directory
      throw new RuntimeException(DEFAULT_PROPERTIES_FILE
          + " can't be loaded from the org.jboss.tools.foundation.core plugin",
          e);
    }
  }

  @Override
  public String getValue(String key) {
    return getValue(key, null);
  }

  static String lookupValue(final String key, final String context, final String version, final Properties properties) {
    String originalKey = getFullKey(key, context, version);
    SimpleHierarchicalVersion v = new SimpleHierarchicalVersion(version);
    boolean proceed = true;
    String value = null;
    while (proceed) {
      String newKey = getFullKey(key, context, v == null? null : v.toString());
      value = properties.getProperty(newKey);
      if (value != null) {
        if (!originalKey.equals(newKey)) {
          //Store found value to speed up next lookup
          properties.put(originalKey, value);
        }
        return value;
      }
      if (v == null) {
        proceed = false;
      } else {
        v = v.getParentVersion();
      }
    }

    //Try without context
    value = properties.getProperty(key);
    if (value != null) {
      properties.put(originalKey, value);
    }
    return value;
  }

  private static String getFullKey(String key, String context, String version) {
    StringBuilder k = new StringBuilder(key);
    k.append("|").append(context);
    if (version != null && !version.isEmpty() ) {
      k.append("|").append(version);
    }
    return k.toString();
  }

  static Properties loadProperties(URI propertiesURI, IProgressMonitor monitor) throws CoreException {
    if (propertiesURI == null) {
      throw new IllegalArgumentException("properties URL can not be null");
    }
    if (monitor == null) {
      monitor = new NullProgressMonitor();
    }

    URLTransportUtility transport = new URLTransportUtility();
    File propFile = transport.getCachedFileForURL(
        propertiesURI.toString(), "Loading IDE properties", 1,
        monitor);

    // XXX propFile is never null, even if the URL is invalid. Sounds fishy
    if (propFile == null || !propFile.canRead() || propFile.length() == 0) {
      throw new CoreException(new Status(IStatus.ERROR,
          FoundationCorePlugin.PLUGIN_ID,
          "Unable to retrieve properties from "
              + propertiesURI));
    }

    try {
      return getProperties(new FileInputStream(propFile));
    } catch (IOException ioe) {
      throw new CoreException(new Status(IStatus.ERROR,
          FoundationCorePlugin.PLUGIN_ID, "Unable to read properties from "
              + propertiesURI));
    }
  }

  private static Properties getProperties(InputStream is) throws IOException {
    Properties props = new Properties();
    try {
      props.load(is);
    } finally {
      closeQuietly(is);
    }
    return props;
  }

  @Override
  public void setInitializationData(IConfigurationElement config, String propertyName, Object data) throws CoreException {
    id = config.getAttribute("id");
    context = config.getAttribute("context");
  }
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((context == null) ? 0 : context.hashCode());
    String curVer = getCurrentVersion();
    result = prime * result
        + ((curVer == null) ? 0 : curVer.hashCode());
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    VersionPropertiesProvider other = (VersionPropertiesProvider) obj;
    if (context == null) {
      if (other.context != null) {
        return false;
      }
    } else if (!context.equals(other.context)) {
      return false;
    }
    String curVer = getCurrentVersion();
    String otherVer = other.getCurrentVersion();
    if (curVer == null) {
      if (otherVer != null) {
        return false;
      }
    } else if (!currentVersion.equals(otherVer)) {
      return false;
    }
    if (id == null) {
      if (other.id != null) {
        return false;
      }
    } else if (!id.equals(other.id)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return "VersionPropertiesProvider [ id=" + id +
        ", propertiesURI=" + propertiesURI +
        ", context=" + getContext() +
        ", currentVersion=" + getCurrentVersion() +
        "]";
  }

  private static void closeQuietly(Closeable closeable) {
    if (closeable != null) {
      try {
        closeable.close();
      } catch (Exception e) {
      }
    }
  }

  private static String lookupKey(final String key, final String context,
      SimpleHierarchicalVersion version, final Properties properties) {
    String value = null;
    String newKey = null;
    SimpleHierarchicalVersion v = version;
    boolean proceed = true;
    while (proceed) {
      newKey = getFullKey(key, context, v == null ? null : v.toString());
      value = properties.getProperty(newKey);
      if (value != null) {
        return newKey;
      }
      if (v == null) {
        proceed = false;
      } else {
        v = v.getParentVersion();
      }
    }

    // Try without context
    value = properties.getProperty(key);
    if (value != null) {
      return key;
    }
    return null;
  }
}

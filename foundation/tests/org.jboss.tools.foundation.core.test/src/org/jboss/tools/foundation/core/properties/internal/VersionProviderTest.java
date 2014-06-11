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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.net.URI;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.jboss.tools.foundation.core.properties.IPropertiesProvider;
import org.jboss.tools.foundation.core.properties.PropertiesHelper;
import org.jboss.tools.foundation.core.properties.mock.MockDevStudioPropertiesProvider;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class VersionProviderTest {

  private static final String CRLF = System.getProperty("line.separator");

  private IProgressMonitor monitor;

  @Before
  public void setUp() {
    monitor = new NullProgressMonitor();
  }

  @After
  public void tearDown() {
    monitor = null;
  }

  @Test
  public void testLoadProperties() throws Exception {
    URI propertiesURI = new File("data/jbosstools-versions.properties").toURI();
    Properties props = VersionPropertiesProvider.loadProperties(propertiesURI , null);
    assertNotNull(props);
    assertTrue(props.size() > 0);
  }


  @Test
  public void testLoadCachedProperties() throws Exception {

    File buildDir = new File("bin");
    if (!buildDir.exists() || !buildDir.isDirectory()) {
      buildDir = new File("target");
    }

    File targetDir = new File(buildDir, "remotelocation");
    FileUtils.deleteDirectory(targetDir);
    targetDir.mkdirs();


    FileUtils.copyFileToDirectory(new File("data/jbosstools-versions.properties"), targetDir);
    File remoteFile = new File(targetDir, "jbosstools-versions.properties");
    assertTrue(remoteFile.exists());

    URI propertiesURI = remoteFile.toURI();
    Properties props = VersionPropertiesProvider.loadProperties(propertiesURI , monitor);
    assertNotNull(props);
    assertTrue(props.size() > 0);

    remoteFile.delete();
    assertFalse(remoteFile.exists());

    Properties cache = VersionPropertiesProvider.loadProperties(propertiesURI , monitor);
    assertNotNull(cache);
    assertEquals(props, cache);


  }


  @Test
  public void testLoadPropertiesViaSysProp() throws Exception {
    URI propertiesURI = new File("data/jbosstools-versions.properties").toURI();
    try {
      System.setProperty(VersionPropertiesProvider.VERSION_PROPERTIES_URI_KEY, propertiesURI.toString());
      VersionPropertiesProvider provider = new VersionPropertiesProvider((String)null,"jbosstools", "4.2.0");
      assertEquals("bar", provider.getValue("foo"));
    } finally {
      System.clearProperty(VersionPropertiesProvider.VERSION_PROPERTIES_URI_KEY);
    }
  }

  @Test
  public void testGetValue() {
    Properties properties = new Properties();
    String value = "bar";
    properties.put("foo|jbosstools|4.1.1", value);
    //versions don't match so no result found
    String result = VersionPropertiesProvider.lookupValue("foo", "jbosstools", "4.1.2.Beta1-v20140211-1204-B52", properties );
    assertNull(result);


    properties.put("foo|jbosstools|4.1.2", value);
    //Contexts don't match so no result found
    result = VersionPropertiesProvider.lookupValue("foo", "devstudio", "4.1.2.Beta1-v20140211-1204-B52", properties );
    assertNull(result);

    //Matching similar version and context so result found
    result = VersionPropertiesProvider.lookupValue("foo", "jbosstools", "4.1.2.Beta1-v20140211-1204-B52", properties );
    assertEquals(value, result);

    value = "woot";
    properties.put("foo|jbosstools|4.1.2.GA", value);
    //Matching exact version so result found
    result = VersionPropertiesProvider.lookupValue("foo", "jbosstools", "4.1.2.GA", properties );
    assertEquals(value, result);

    value = "baz";
    properties.put("foo|devstudio|8.3", value);
    //Matching similar version so result found
    result = VersionPropertiesProvider.lookupValue("foo", "devstudio", "8.3.2.CR1-v20140211-1204-B52", properties );
    assertEquals(value, result);

    value = "buzz";
    //Matching global version so result found
    properties.put("foo|devstudio|8", value);
    result = VersionPropertiesProvider.lookupValue("foo", "devstudio", "8.4.2.CR1-v20140211-1204-B52", properties );
    assertEquals(value, result);

    value = "meh";
    //Matching global context so result found
    properties.put("foo|eclipse", value);
    result = VersionPropertiesProvider.lookupValue("foo", "eclipse", "8.4.2.CR1-v20140211-1204-B52", properties );
    assertEquals(value, result);

    value = "buh";
    //Matching universal key so result found
    properties.put("foo", value);
    long start1 = System.nanoTime();
    result = VersionPropertiesProvider.lookupValue("foo", "unknown", "8.4.2.CR1-v20140211-1204-B52", properties );
    long elapsed1 = System.nanoTime() - start1;
    assertEquals(value, result);

    long start2 = System.nanoTime();
    //Check subsequent lookups are faster
    result = VersionPropertiesProvider.lookupValue("foo", "unknown", "8.4.2.CR1-v20140211-1204-B52", properties );
    long elapsed2 = System.nanoTime() - start2;
    String msg = "2nd lookup took "+ elapsed2 +" ns, compared to the first one : "+ elapsed1 +" ns";
    //System.err.println(msg);
    assertEquals(value, result);
    assertTrue(msg, elapsed1 > elapsed2);
  }


  @Test
  public void testDefaultPropertiesProvider() throws Exception {
    IPropertiesProvider propertiesProvider = PropertiesHelper.getPropertiesProvider();
    assertNotNull("Default IPropertiesProvider not found", propertiesProvider);

    String discoveryUrl = propertiesProvider
        .getValue("jboss.discovery.directory.url");
    // System.err.println("discoveryUrl :"+discoveryUrl);
    assertNotNull(discoveryUrl);

    String discoverySiteUrl = propertiesProvider
        .getValue("jboss.discovery.site.url");
    // System.err.println("discoverySiteUrl :"+discoverySiteUrl);
    assertNotNull(discoverySiteUrl);
  }

  @Test
  public void testGetDefaultValue() throws Exception {
    Properties props = new Properties();
    VersionPropertiesProvider propertiesProvider = new VersionPropertiesProvider(props ,"jbosstools", "4.2.0");

    String result = propertiesProvider.getValue("foo", "bar");
    assertEquals("bar", result);

    //Check previous default value was not cached
    result = propertiesProvider.getValue("foo");
    assertNull(result);
  }

  @Test
  public void testFallBackEmbeddedValues() throws Exception {
    VersionPropertiesProvider provider = new MockDevStudioPropertiesProvider();
    assertNotNull(provider.getValue("jboss.discovery.site.url"));
  }

  @Test
  public void testDumpProperties() throws Exception {
    Properties props = new Properties();
    props.put("foo", "bar");
    props.put("foo|jbosstools|8", "bar");
    props.put("foo|devstudio|8", "bar");
    props.put("foo|devstudio|7.0", "bar");
    props.put("foo|devstudio|8.0.0.Beta2", "bar");
    props.put("foo|devstudio|8.0.1", "bar");
    props.put("woo|devstudio|8.0", "kie");
    props.put("yee", "haa");
    props.put("context|devstudio", "bar");
    VersionPropertiesProvider provider = new VersionPropertiesProvider(
        (Properties) null, "devstudio", "8.0.0.Beta2");
    String output = provider.dump(props);
    // System.err.println(output);
    assertNotContains("foo=bar", output);
    assertNotContains("foo|jbosstools|8=bar", output);
    assertNotContains("foo|devstudio|8=bar", output);
    assertNotContains("foo|devstudio|7.0=bar", output);
    assertContains("foo|devstudio|8.0.0.Beta2=bar", output);
    assertNotContains("foo|devstudio|8.0.1=bar", output);
    assertContains("woo|devstudio|8.0=kie", output);
    assertContains("yee=haa", output);
    assertContains("context|devstudio=bar", output);

  }

  private void assertContains(String expectedString, String content) {
    assertTrue("'" + expectedString + "' was expected in : " + CRLF + content,
        content.contains(expectedString));
  }

  private void assertNotContains(String unexpectedString, String content) {
    assertFalse("'" + unexpectedString + "' was unexpected in :" + CRLF
        + content,
        content.contains(unexpectedString));
  }
}

package org.jboss.tools.foundation.core.properties.mock;

import org.jboss.tools.foundation.core.properties.IPropertiesProvider;

public class MockVersionProvider implements IPropertiesProvider {

  @Override
  public String getValue(String key) {
    return null;
  }

  @Override
  public String getValue(String key, String defaultValue) {
    return defaultValue;
  }

}

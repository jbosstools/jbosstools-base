package org.jboss.tools.common.databinding;

import java.beans.PropertyChangeListener;

public interface IObservablePojo {

	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener);

	public void addPropertyChangeListener(PropertyChangeListener listener);

	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener);

	public void removePropertyChangeListener(
			PropertyChangeListener listener);

	public void removeAllPropertyChangeListeners();

	public void dispose();

}
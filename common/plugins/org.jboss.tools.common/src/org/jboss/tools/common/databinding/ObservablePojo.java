/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.databinding;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * A POJO base class that may notify observers on behalf of
 * {@link PropertyChangeSupport}
 */
public abstract class ObservablePojo {

	private PropertyChangeSupport propertyChangeSupport;

	public ObservablePojo() {
		this.propertyChangeSupport = new PropertyChangeSupport(this);
	}

	public void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
		propertyChangeSupport.firePropertyChange(propertyName, oldValue, newValue);
	}
	
    public void fireIndexedPropertyChange(String propertyName, int index, Object oldValue, Object newValue) {
    	propertyChangeSupport.fireIndexedPropertyChange(propertyName, index, oldValue, newValue);
    }
	
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		if (!contains(listener)) {
			propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
		}
	}

	public void addPropertyChangeListener(PropertyChangeListener listener) {
		if (!contains(listener)) {
			propertyChangeSupport.addPropertyChangeListener(listener);
		}
	}

	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
	}

	public void removePropertyChangeListener(
			PropertyChangeListener listener) {
		propertyChangeSupport.removePropertyChangeListener(listener);
	}
	
    protected boolean contains(PropertyChangeListener listener) {
    	boolean contains = false;
    	for (PropertyChangeListener registeredListener : propertyChangeSupport.getPropertyChangeListeners()) {
    		if (registeredListener == listener) {
    			contains = true;
    			break;
    		}
    	}
    	return contains;
    }
    
	protected PropertyChangeSupport getPropertyChangeSupport() {
		return propertyChangeSupport;
	}
}

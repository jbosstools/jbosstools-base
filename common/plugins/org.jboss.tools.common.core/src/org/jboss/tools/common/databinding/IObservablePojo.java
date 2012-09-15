/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/

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
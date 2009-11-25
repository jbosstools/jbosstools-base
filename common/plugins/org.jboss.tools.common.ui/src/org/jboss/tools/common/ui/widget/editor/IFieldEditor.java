/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.ui.widget.editor;

import java.beans.PropertyChangeListener;

import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.widgets.Listener;

public interface IFieldEditor extends INamedElement {
	
	/**
	 * 
	 * @param composite
	 */
	public Object[] getEditorControls(Object composite);

	/**
	 * 
	 * @return
	 */
	public Object[] getEditorControls();

	/**
	 * 
	 * @return
	 */
	public int getNumberOfControls();

	/**
	 * 
	 * @param parent
	 */
	public void doFillIntoGrid(Object parent);

	/**
	 * 
	 * @param listener
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener);

	/**
	 * 
	 * @param listener
	 */
	public void addDisposeListener(DisposeListener listener);

	/**
	 * 
	 * @param listener
	 */
	public void removeDisposeListener(DisposeListener listener);

	/**
	 * 
	 * @param listener
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener);

	/**
	 * 
	 * @return
	 */
	public boolean isEditable();

	/**
	 * 
	 * @param aEdiatble
	 */
	public void setEditable(boolean aEdiatble);

	/**
	 * @return
	 * 
	 */
	public boolean setFocus();

	/**
	 * 
	 * @return
	 */
	public boolean isEnabled();

	/**
	 * 
	 * @param enabled
	 */
	public void setEnabled(boolean enabled);
	
	/**
	 * 
	 */
	public void dispose();

	/**
	 * 
	 * @param e
	 */
	public void dispose(DisposeEvent e);

	/**
	 * Sets the application defined property of this editor
	 * @param data
	 * @param key
	 */
	public void setData(Object key, Object data);

	/**
	 * Gets the application defined property of this editor
	 * @param key
	 * @return
	 */
	public Object getData(Object key);
}
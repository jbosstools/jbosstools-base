/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.model.ui.viewers.xpl;

/**
 * @author au
 */

public interface ICheckable {
	public static final int STATE_UNCHECK = 0;
	public static final int STATE_CHECK = 1;
	public static final int STATE_HALFCHECK = 2;
	
	public void addCheckStateListener(ICheckStateListener listener);
	public int getState(Object element);
	public void removeCheckStateListener(ICheckStateListener listener);
	public boolean setState(Object element, int state);
	public void toggle(Object element);
}

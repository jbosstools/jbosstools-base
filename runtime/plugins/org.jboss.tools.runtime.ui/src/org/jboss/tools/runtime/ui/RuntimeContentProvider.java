/*************************************************************************************
 * Copyright (c) 2010-2011 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.runtime.ui;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.jboss.tools.runtime.core.model.ServerDefinition;

/**
 * @author snjeza
 * 
 */
public class RuntimeContentProvider implements ITreeContentProvider {

	private List<ServerDefinition> serverDefinitions;

	public RuntimeContentProvider(List<ServerDefinition> serverDefinitions) {
		this.serverDefinitions = serverDefinitions;
	}
	
	public Object[] getElements(Object inputElement) {
		return serverDefinitions.toArray();
	}

	public void dispose() {

	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		serverDefinitions = (List<ServerDefinition>) newInput;
	}

	public boolean hasChildren(Object element) {
		return ((ServerDefinition) element).getIncludedServerDefinitions().size() > 0;
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		List<ServerDefinition> list = ((ServerDefinition) parentElement).getIncludedServerDefinitions();
		return list.toArray();
	}

	@Override
	public Object getParent(Object element) {
		return ((ServerDefinition) element).getParent();
	}
}

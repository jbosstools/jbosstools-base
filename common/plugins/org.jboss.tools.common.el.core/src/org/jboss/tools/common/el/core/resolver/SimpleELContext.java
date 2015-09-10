/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IRegion;
import org.jboss.tools.common.el.core.ELReference;

/**
 * @author Alexey Kazakov
 */
public class SimpleELContext implements ELContext {
	static final ELReference[] EMPTY_ARRAY = new ELReference[0];

	protected IFile resource;
	protected boolean dirty;
	protected ELResolver[] elResolvers;
	protected List<Var> vars = new ArrayList<Var>();

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getElResolvers()
	 */
	public ELResolver[] getElResolvers() {
		return elResolvers;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#setElResolvers(org.jboss.tools.common.el.core.resolver.ELResolver[])
	 */
	public void setElResolvers(ELResolver[] elResolvers) {
		this.elResolvers = elResolvers;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getResource()
	 */
	public IFile getResource() {
		return resource;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#setResource(org.eclipse.core.resources.IFile)
	 */
	public void setResource(IFile resource) {
		this.resource = resource;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars()
	 */
	public Var[] getVars() {
		List<Var> vars = getVarsAsList();
		return vars.toArray(new Var[vars.size()]);
	}

	public List<Var> getVarsAsList() {
		return vars;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#setVars(java.util.List)
	 */
	public void setVars(List<Var> vars) {
		this.vars = vars;
	}

	/**
	 * Adds new Var to the context
	 * @param vars
	 */
	public void addVar(Var var) {
		vars.add(var);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getELReferences()
	 */
	public ELReference[] getELReferences() {
		return EMPTY_ARRAY;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars(int)
	 */
	public Var[] getVars(int offset) {
		return getVars();
	}

	public List<Var> getVarsAsList(int offset) {
		return getVarsAsList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getELReference(int)
	 */
	public ELReference getELReference(int offset) {
		return null;
	}

	@Override
	public Collection<ELReference> getELReferences(IRegion region) {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return dirty;
	}

	public void setDirty(boolean dirty) {
		this.dirty = dirty;
	}
}
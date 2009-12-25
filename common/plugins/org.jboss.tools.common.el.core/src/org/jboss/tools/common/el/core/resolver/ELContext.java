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

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.jboss.tools.common.el.core.ELReference;

/**
 * @author Alexey Kazakov
 */
public interface ELContext {

	/**
	 * Returns Resource of the page
	 * @return
	 */
	IFile getResource();

	/**
	 * Returns "var" attributes which are available in particular offset or the all "var"s if offset == -1 
	 * 
	 * @param offset
	 * @return
	 */
	Var[] getVars(int offset);

	/**
	 * Returns all EL references of the file of this context.
	 * 
	 * @return
	 */
	ELReference[] getELReferences();

	/**
	 * Returns the EL reference by the offset.
	 * 
	 * @return
	 */
	ELReference getELReference(int offset);

	/**
	 * Returns EL Resolvers which are declared for this resource
	 * @return
	 */
	ELResolver[] getElResolvers();

	/**
	 * Sets the var list
	 * @param vars
	 */
	void setVars(List<Var> vars);

	/**
	 * Sets the resolvers
	 * @param resolvers
	 */
	void setElResolvers(ELResolver[] resolvers);

	/**
	 * Sets the source file
	 * @param file
	 */
	void setResource(IFile file);
}
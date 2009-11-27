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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.text.Region;

/**
 * EL context
 * @author Alexey Kazakov
 */
public class ELContextImpl extends SimpleELContext {

	protected Map<Region, List<Var>> vars = new HashMap<Region, List<Var>>();
	protected List<Var> allVars = new ArrayList<Var>();

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars()
	 */
	public Var[] getVars() {
		return allVars.toArray(new Var[allVars.size()]);
	}

	/**
	 * Adds new Var to the context
	 * @param region
	 * @param vars
	 */
	public void addVar(Region region, Var var) {
		if (this.vars.get(region) == null) {
			this.vars.put(region, new ArrayList<Var>());
		}
		this.vars.get(region).add(var);
		allVars.add(var);
	}
}
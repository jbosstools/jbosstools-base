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
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.jboss.tools.common.el.core.ELReference;

/**
 * EL context
 * @author Alexey Kazakov
 */
public class ELContextImpl extends SimpleELContext {
	static List<Var> EMPTY = Collections.<Var>emptyList();

	protected List<Var> allVars = new ArrayList<Var>();
	protected ELReference[] elReferences;
	protected List<ELReference> elReferenceSet;

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars()
	 */
	@Override
	public Var[] getVars() {
		List<Var> external = getExternalVars();
		if(external.isEmpty()) {
			return allVars.toArray(new Var[allVars.size()]);
		} else if(allVars.isEmpty()) {
			return external.toArray(new Var[allVars.size()]);
		}
		ArrayList<Var> result = new ArrayList<Var>();
		result.addAll(allVars);
		result.addAll(external);
		return result.toArray(new Var[allVars.size()]);
	}

	public List<Var> getExternalVars() {
		return EMPTY;
	}

	/**
	 * Adds new Var to the context
	 * @param region
	 * @param vars
	 */
	public void addVar(Region region, Var var) {
		var.setRegion(region);
		allVars.add(var);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars(int)
	 */
	@Override
	public Var[] getVars(int offset) {
		if(offset<0) {
			return getVars();
		}
		List<Var> result = new ArrayList<Var>();
		for (Var var : allVars) {
			Region region = var.getRegion();
			if(offset>=region.getOffset() && offset<=region.getOffset() + region.getLength()) {
				result.add(var);
			}
		}
		List<Var> external = getExternalVars();
		if(!external.isEmpty()) {
			result.addAll(external);
		}
		return result.toArray(new Var[result.size()]);
	}

	/**
	 * @return the allVars
	 */
	public List<Var> getAllVars() {
		return allVars;
	}

	/**
	 * @param allVars the allVars to set
	 */
	public void setAllVars(List<Var> allVars) {
		this.allVars = allVars;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.IXmlContext#getELReferences()
	 */
	@Override
	public ELReference[] getELReferences() {
		if(elReferences==null) {
			if(elReferenceSet==null || elReferenceSet.isEmpty()) {
				return EMPTY_ARRAY;
			}
			elReferences = elReferenceSet.toArray(new ELReference[0]);
		}
		return elReferences;
	}

	public void addELReference(ELReference reference) {
		if(elReferenceSet==null) {
			elReferenceSet = new ArrayList<ELReference>();
		}
		elReferenceSet.add(reference);
		elReferences = null;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.SimpleELContext#getELReference(int)
	 */
	@Override
	public ELReference getELReference(int offset) {
		if(elReferenceSet != null) {
			for (ELReference ref: elReferenceSet) {
				if(ref.getStartPosition()<=offset && (ref.getStartPosition() + ref.getLength()>offset)) {
					return ref;
				}
			}
		}
		return null;
	}

	@Override
	public Set<ELReference> getELReferences(IRegion region) {
		Set<ELReference> references = new HashSet<ELReference>();
		if(elReferenceSet != null) {
			for (ELReference ref: elReferenceSet) {
				if(region.getOffset() + region.getLength() >= ref.getStartPosition() && region.getOffset() <= ref.getStartPosition() + ref.getLength()) {
					references.add(ref);
				}
			}
		}
		return references;
	}
}
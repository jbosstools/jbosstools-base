package org.jboss.tools.common.el.core.resolver;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.Region;

/**
 * EL context
 * @author Alexey Kazakov
 */
public class ELContextImpl implements ELContext {

	protected IFile resource;
	protected ELResolver[] elResolvers;
	protected ElVarSearcher varSearcher;
	protected Map<Region, Set<Var>> vars = new HashMap<Region, Set<Var>>();
	protected Set<Var> allVars = new HashSet<Var>();

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.kb.text.PageContext#getElResolvers()
	 */
	public ELResolver[] getElResolvers() {
		return elResolvers;
	}

	public void setElResolvers(ELResolver[] elResolvers) {
		this.elResolvers = elResolvers;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.kb.text.PageContext#getResource()
	 */
	public IFile getResource() {
		return resource;
	}

	public void setResource(IFile resource) {
		this.resource = resource;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVarSearcher()
	 */
	public ElVarSearcher getVarSearcher() {
		return varSearcher;
	}

	/**
	 * @param varSearcher the varSearcher to set
	 */
	public void setVarSearcher(ElVarSearcher varSearcher) {
		this.varSearcher = varSearcher;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELContext#getVars()
	 */
	public Var[] getVars() {
		return allVars.toArray(new Var[allVars.size()]);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.kb.text.PageContext#getVars(int)
	 */
	public Var[] getVars(int offset) {
		Set<Var> result = new HashSet<Var>();
		for (Region region : vars.keySet()) {
			if(offset>=region.getOffset() && offset<=region.getOffset() + region.getLength()) {
				result.addAll(vars.get(region));
			}
		}
		return result.toArray(new Var[result.size()]);
	}

	/**
	 * Adds new Var to the context
	 * @param region
	 * @param vars
	 */
	public void addVar(Region region, Var var) {
		this.vars.get(region).add(var);
		allVars.add(var);
	}
}
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

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.ElCoreMessages;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * @author Alexey Kazakov
 */
public class ELSegmentImpl implements ELSegment {

	protected IResource resource;
	protected ITextSourceReference sourceReference;
	protected LexicalToken token;
	protected boolean resolved = false;
	protected boolean validatable = true;
	protected List<IVariable> variables = new ArrayList<IVariable>();
	protected List<Var> vars = new ArrayList<Var>();
	

	public ELSegmentImpl(LexicalToken token) {
		this.token = token;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getResource()
	 */
	public IResource getResource() {
		return resource;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getSourceReference()
	 */
	public ITextSourceReference getSourceReference() {
		if(sourceReference==null) {
			sourceReference = new ITextSourceReference() {
				public int getStartPosition() {
					return token.getStart();
				}
				public int getLength() {
					return token.getLength();
				}
				public IResource getResource() {
					return resource;
				}
			};
		}
		return sourceReference;
	}

	public static class VarOpenable implements IOpenableReference {
		Var var;
		VarOpenable(Var var) {
			this.var = var;
		}
		@Override
		public boolean open() {
			IEditorPart part = null;
			IWorkbenchWindow window = ELCorePlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
			if (window == null)	return false;
			IWorkbenchPage page = window.getActivePage();
			try {
				part = IDE.openEditor(page, var.getFile());
			} catch (PartInitException e) {
				ELCorePlugin.getDefault().logError(e);
			}
			if(part != null) {
				part.getEditorSite().getSelectionProvider().setSelection(new TextSelection(var.getDeclarationOffset(), var.getDeclarationLength()));
			}
			return false;
		}

		@Override
		public String getLabel() {
			return MessageFormat.format(ElCoreMessages.OpenVarDefinition, var.getName(), var.getFile().getName());
		}
	
		public Var getVar() {
			return var;
		}

		@Override
		public Image getImage() {
			return null;
		}
	}
	
	/**
	 * Default empty implementation. Subclasses should override this method.
	 * 
	 * @return
	 */
	public IOpenableReference[] getOpenable() {
		if(!vars.isEmpty()) {
			List<IOpenableReference> rs = new ArrayList<IOpenableReference>();
			for (Var var: vars) {
				rs.add(new VarOpenable(var));
			}
			return rs.toArray(new VarOpenable[0]);
		}
		return new IOpenableReference[0];
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#getToken()
	 */
	public LexicalToken getToken() {
		return token;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#isResolved()
	 */
	public boolean isResolved() {
		return resolved;
	}

	/**
	 * @param resource the resource to set
	 */
	public void setResource(IResource resource) {
		this.resource = resource;
	}

	/**
	 * @param sourceReference the sourceReference to set
	 */
	public void setSourceReference(ITextSourceReference sourceReference) {
		this.sourceReference = sourceReference;
	}

	/**
	 * @param token the token to set
	 */
	public void setToken(LexicalToken token) {
		this.token = token;
	}

	/**
	 * @param resolved the resolved to set
	 */
	public void setResolved(boolean resolved) {
		this.resolved = resolved;
	}

	/**
	 * @return the variables
	 */
	public List<IVariable> getVariables() {
		return variables;
	}

	/**
	 * @param variable the variables to set
	 */
	public void setVariables(List<IVariable> variables) {
		this.variables = variables;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return token!=null?token.getText() + "(" + resolved + ")": super.toString(); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELSegment#isValidatable()
	 */
	@Override
	public boolean isValidatable() {
		return validatable;
	}

	/**
	 * @param validatable the validatable to set
	 */
	public void setValidatable(boolean validatable) {
		this.validatable = validatable;
	}

	public void setVars(List<Var> vars) {
		this.vars = vars;
	}

	public Var getVar() {
		return vars.isEmpty() ? null : vars.iterator().next();
	}
}
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
package org.jboss.tools.common.validation;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.wst.sse.ui.internal.reconcile.validator.ISourceValidator;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;

/**
 * This Manager is responsible for as-you-type validation.
 * It's registred as WTP source validator and delegates validation to the corresponding JBT validators.
 * @author Alexey Kazakov
 */
public class AsYouTypeValidatorManager implements ISourceValidator, org.eclipse.wst.validation.internal.provisional.core.IValidator {

	private IDocument document;
	private IFile file;
	private EditorValidationContext context;
	private Map<IValidator, IProject> rootProjects;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.sse.ui.internal.reconcile.validator.ISourceValidator#connect(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public void connect(IDocument document) {
		this.document = document;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.sse.ui.internal.reconcile.validator.ISourceValidator#disconnect(org.eclipse.jface.text.IDocument)
	 */
	@Override
	public void disconnect(IDocument document) {
		context = null;
	}

	private boolean init(IValidationContext helper, IReporter reporter) {
		if(context==null) {
			String[] uris = helper.getURIs();
			if(uris.length==0) {
				return false;
			}
			IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
			file = root.getFile(new Path(uris[0]));
			if(!file.isAccessible()) {
				return false;
			}
			context = new EditorValidationContext(file.getProject(), document);
			if(context.getValidators().isEmpty()) {
				return false;
			}
			rootProjects = new HashMap<IValidator, IProject>();
			for (IValidator validator : context.getValidators()) {
				Map<IProject, IValidatingProjectSet> projectTree = context.getValidatingProjectTree(validator).getBrunches();
				if(!projectTree.isEmpty()) {
					IProject rootProject = projectTree.keySet().iterator().next();
					rootProjects.put(validator, rootProject);
				}
			}
		}
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.wst.sse.ui.internal.reconcile.validator.ISourceValidator#validate(org.eclipse.jface.text.IRegion, org.eclipse.wst.validation.internal.provisional.core.IValidationContext, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	@Override
	public void validate(IRegion dirtyRegion, IValidationContext helper, IReporter reporter) {
		if(!init(helper, reporter)) {
			return;
		}
		for (IValidator validator : context.getValidators()) {
			IProject rootProject = rootProjects.get(validator);
			IValidatingProjectSet projectBrunch = context.getValidatingProjectTree(validator).getBrunches().get(rootProject);
			if(projectBrunch!=null) {
				((IAsYouTypeValidator)validator).validate(this, rootProject, dirtyRegion, helper, reporter, context, projectBrunch.getRootContext(), file);
			}
		}
//		reporter.removeAllMessages(this, file);
	}

	@Override
	public void cleanup(IReporter reporter) {
	}

	@Override
	public void validate(IValidationContext helper, IReporter reporter)	throws ValidationException {
	}
}
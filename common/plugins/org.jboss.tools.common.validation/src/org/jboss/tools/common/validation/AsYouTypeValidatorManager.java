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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.texteditor.AbstractMarkerAnnotationModel;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.eclipse.wst.sse.ui.internal.reconcile.validator.ISourceValidator;
import org.eclipse.wst.validation.internal.core.ValidationException;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.jboss.tools.common.util.EclipseUIUtil;

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

	private static Set<IDocument> reporters = new HashSet<IDocument>();

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
		synchronized (reporters) {
			reporters.remove(document);
		}
	}

	/**
	 * 	Remove all the temporary annotations added by this validator.
	 */
	static void removeMessages() {
        UIJob job = new UIJob("Removing as-you-type JBT validation problems") {
			public IStatus runInUIThread(IProgressMonitor monitor) {
				ITextEditor e = EclipseUIUtil.getActiveEditor();
				if(e!=null && !e.isDirty()) {
					IEditorInput input = e.getEditorInput();
					IDocumentProvider dp = e.getDocumentProvider();
					IDocument doc = dp.getDocument(input);
					boolean ok = false;
					synchronized (reporters) {
						ok = reporters.contains(doc);
					}
					if(ok) {
						IAnnotationModel model = dp.getAnnotationModel(input);
						if(model instanceof AbstractMarkerAnnotationModel) {
							AbstractMarkerAnnotationModel anModel = ((AbstractMarkerAnnotationModel)model);
							synchronized (anModel.getLockObject()) {
								Iterator iterator = anModel.getAnnotationIterator();
								while (iterator.hasNext()) {
									Object o = iterator.next();
									if(o instanceof TemporaryAnnotation) {
										TemporaryAnnotation annotation = (TemporaryAnnotation)o;
										Map attributes = annotation.getAttributes();
										if(attributes!=null && attributes.get(TempMarkerManager.AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE)!=null) {
											anModel.removeAnnotation(annotation);
										}
									} else if(o instanceof DisabledAnnotation) {
										DisabledAnnotation annotation = (DisabledAnnotation)o;
										anModel.removeAnnotation(annotation);
									}
								}
							}
						}
					}
				}
				return Status.OK_STATUS;
			}
		};
		job.schedule();
	}

	private boolean init(IValidationContext helper, IReporter reporter) {
		if(context==null) {
			synchronized (reporters) {
				reporters.add(document);
			}
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
			if(validator instanceof IAsYouTypeValidator) {
				IProject rootProject = rootProjects.get(validator);
				IValidatingProjectSet projectBrunch = context.getValidatingProjectTree(validator).getBrunches().get(rootProject);
				if(projectBrunch!=null) {
					((IAsYouTypeValidator)validator).validate(this, rootProject, dirtyRegion, helper, reporter, context, projectBrunch.getRootContext(), file);
				}
			}
		}
//		reporter.removeAllMessages(AsYouTypeValidatorManager.this, file);
	}

	@Override
	public void cleanup(IReporter reporter) {
	}

	@Override
	public void validate(IValidationContext helper, IReporter reporter)	throws ValidationException {
	}
}
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

import java.text.MessageFormat;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.texteditor.AbstractMarkerAnnotationModel;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.wst.validation.internal.core.Message;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.text.ITextSourceReference;
import org.jboss.tools.common.util.EclipseUIUtil;

/**
 * @author Alexey Kazakov
 */
abstract public class TempMarkerManager extends ValidationErrorManager {

	public static final String MESSAGE_TYPE_ATTRIBUTE_NAME = "jbt.type";

	protected boolean asYouTypeValidation;
	protected int messageCounter;

	protected abstract String getMessageBundleName();

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.validation.ValidationErrorManager#init(org.eclipse.core.resources.IProject, org.jboss.tools.common.validation.ContextValidationHelper, org.jboss.tools.common.validation.IProjectValidationContext, org.eclipse.wst.validation.internal.provisional.core.IValidator, org.eclipse.wst.validation.internal.provisional.core.IReporter, boolean)
	 */
	@Override
	public void init(IProject project, ContextValidationHelper validationHelper, IProjectValidationContext validationContext, IValidator manager, IReporter reporter, boolean asYouTypeValidation) {
		super.init(project, validationHelper, validationContext, manager, reporter, asYouTypeValidation);
		messageCounter = 0;
	}

	/**
	 * @return the asYouTypeValidation
	 */
	public boolean isAsYouTypeValidation() {
		return asYouTypeValidation;
	}

	/**
	 * @param asYouTypeValidation the asYouTypeValidation to set
	 */
	public void setAsYouTypeValidation(boolean asYouTypeValidation) {
		this.asYouTypeValidation = asYouTypeValidation;
	}

	public void addProblem(String message, String preferenceKey, ITextSourceReference location, IResource target) {
		if(asYouTypeValidation) {
			addMessage(target, location, preferenceKey, message);
		} else {
			addError(message, preferenceKey, location, target);
		}
	}

	public void addProblem(String message, String preferenceKey, String[] messageArguments, ITextSourceReference location, IResource target) {
		if(asYouTypeValidation) {
			addMessage(target, location, preferenceKey, message, messageArguments);
		} else {
			addError(message, preferenceKey, messageArguments, location, target);
		}
	}

	public void addProblem(String message, String preferenceKey, ITextSourceReference location, IResource target, Integer quickFixId) {
		if(asYouTypeValidation) {
			addMessage(target, location, preferenceKey, message, quickFixId);
		} else {
			addError(message, preferenceKey, location, target, quickFixId);
		}
	}

	public IMarker addProblem(String message, String preferenceKey, String[] messageArguments, int length, int offset, IResource target, Integer quickFixId) {
		if(asYouTypeValidation) {
			addMessage(target, offset, length, preferenceKey, message, messageArguments, quickFixId);
			return null;
		} else {
			return addError(message, preferenceKey, messageArguments, length, offset, target, quickFixId);
		}
	}

	public IMarker addProblem(String message, String preferenceKey, String[] messageArguments, int length, int offset, IResource target) {
		if(asYouTypeValidation) {
			addMessage(target, offset, length, preferenceKey, message, messageArguments);
			return null;
		} else {
			return addError(message, preferenceKey, messageArguments, length, offset, target);
		}
	}

	public IMessage addMessage(IResource target, ITextSourceReference location, String preferenceKey, String textMessage) {
		return addMessage(target, -1, location, preferenceKey, textMessage, null);
	}

	public IMessage addMessage(IResource target, ITextSourceReference location, String preferenceKey, String textMessage, String[] messageArguments) {
		return addMessage(target, -1, location, preferenceKey, textMessage, messageArguments);
	}

	public IMessage addMessage(IResource target, ITextSourceReference location, String preferenceKey, String textMessage, Integer quickFixId) {
		IMessage message = addMessage(target, -1, location, preferenceKey, textMessage, null);
		if(message!=null && quickFixId != -1) {
			message.setAttribute(MESSAGE_ID_ATTRIBUTE_NAME, quickFixId);
		}
		return message;
	}

	public IMessage addMessage(IResource target, int lineNumber, ITextSourceReference location, String preferenceKey, String textMessage, String[] messageArguments) {
		IMessage message = null;
		IResource actualTarget = location.getResource();
		if(target.equals(actualTarget)) {
			int severity = getSeverity(preferenceKey, target);
			try {
				if(severity!=-1 && (severity!=IMessage.NORMAL_SEVERITY || !hasSuppressWarningsAnnotation(preferenceKey, location))) {
					message = addMesssage(target, lineNumber, location.getStartPosition(), location.getLength(), severity, preferenceKey, textMessage, messageArguments);
				}
			} catch (JavaModelException e) {
				CommonPlugin.getDefault().logError(e);
			}
		}
		return message;
	}

	public IMessage addMessage(IResource target, int offset, int length, String preferenceKey, String messageText, String[] messageArguments, int quickFixId) {
		return addMessage(target, -1, offset, length, preferenceKey, messageText, messageArguments, quickFixId);
	}

	public IMessage addMessage(IResource target, int offset, int length, String preferenceKey, String message, String[] messageArguments) {
		return addMessage(target, -1, offset, length, preferenceKey, message, messageArguments, -1);
	}

	public IMessage addMessage(IResource target, int lineNumber, int offset, int length, String preferenceKey, String message, String[] messageArguments, int quickFixId) {
		int severity = getSeverity(preferenceKey, target);
		return severity!=-1?addMesssage(target, lineNumber, offset, length, severity, preferenceKey, message, messageArguments):null;
	}
	
	private IMessage addMesssage(IResource target, int lineNumber, int offset, int length, int severity, String preferenceKey, String textMessage, String[] messageArguments) {
		IMessage message = null;
		if(messageCounter<=getMaxNumberOfMarkersPerFile(target.getProject())) {
			if(lineNumber<0) {
				try {
					lineNumber = document.getLineOfOffset(offset) + 1;
				} catch (BadLocationException e) {
					CommonPlugin.getDefault().logError(e);
				}
			}
			message = addMesssage(validationManager, shouldCleanAllAnnotations(), this.reporter, offset, length, target, lineNumber, severity, textMessage, messageArguments, getMessageBundleName());
			messageCounter++;
			String preferencePageId = getPreferencePageId();
			if(preferencePageId != null && preferenceKey != null){
				message.setAttribute(PREFERENCE_KEY_ATTRIBUTE_NAME, preferenceKey);
				message.setAttribute(PREFERENCE_PAGE_ID_NAME, preferencePageId);
			}
			String type = getMarkerType();
			if(type!=null) {
				message.setAttribute(MESSAGE_TYPE_ATTRIBUTE_NAME, type);
			}
		}
		return message;
	}

	/**
	 * Returns true if all the annotations belonged to this validator of entire document should be removed before start this validator again.
	 * If "false" then only annotations of the changed region should be removed.
	 * Returns "false" by default but subclasses are free to override this method. 
	 * @return
	 */
	protected boolean shouldCleanAllAnnotations() {
		return false;
	}

	public static final String AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE = "org.jboss.tools.common.validation.asyoutype";
	public static final String CLEAN_ALL_ANNOTATIONS_ATTRIBUTE = "org.jboss.tools.common.validation.cleanAllAnnotaions";

	private static IMessage addMesssage(IValidator validator, boolean cleanAllAnnotaions, IReporter reporter, int offset, int length, IResource target, int lineNumber, int severity, String textMessage, Object[] messageArguments, String bundleName) {
		Message message = new ValidationMessage(severity, messageArguments!=null?MessageFormat.format(textMessage, messageArguments):textMessage, target);
		message.setOffset(offset);
		message.setLength(length);
		message.setLineNo(lineNumber);
		message.setBundleName(bundleName);
		message.setAttribute(AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE, Boolean.TRUE);
		if(cleanAllAnnotaions) {
			message.setAttribute(CLEAN_ALL_ANNOTATIONS_ATTRIBUTE, Boolean.TRUE);
		}
		reporter.addMessage(validator, message);
		return message;
	}

	protected void disableProblemAnnotations(final ITextSourceReference reference) {
        UIJob job = new UIJob("As-you-type JBT validation. Disabling the marker annotations.") {
			public IStatus runInUIThread(IProgressMonitor monitor) {
				if(EclipseUIUtil.isActiveEditorDirty()) {
					ITextEditor e = EclipseUIUtil.getActiveEditor();
					IEditorInput input = e.getEditorInput();
					IDocumentProvider dp = e.getDocumentProvider();
					if(document == dp.getDocument(input)) {
						IAnnotationModel model = dp.getAnnotationModel(input);
						if(model instanceof AbstractMarkerAnnotationModel) {
							AbstractMarkerAnnotationModel anModel = ((AbstractMarkerAnnotationModel)model);
							synchronized (anModel.getLockObject()) {
								Iterator iterator = anModel.getAnnotationIterator(reference.getStartPosition(), reference.getLength(), true, true);
								Set<MarkerAnnotation> annotationsToRemove = new HashSet<MarkerAnnotation>();
								Map<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
								while (iterator.hasNext()) {
									Object o = iterator.next();
									if(o instanceof MarkerAnnotation) {
										MarkerAnnotation annotation = (MarkerAnnotation)o;
										IMarker marker = annotation.getMarker();
										try {
											String type = marker.getType();
											if(getMarkerType().equals(type)) {
												Annotation newAnnotation = new DisabledAnnotation(annotation.getType(), false, annotation.getText(), marker.getAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING) == IMarker.SEVERITY_WARNING);
												int offset = marker.getAttribute(IMarker.CHAR_START, 0);
												int length = 0; // marker.getAttribute(IMarker.CHAR_END, 0) - offset;
												Position p = new Position(offset, length);
												newAnnotations.put(newAnnotation, p);
												annotationsToRemove.add(annotation);
											} else if("org.jboss.tools.jst.jsp.jspeditor.JSPMultiPageEditor".equals(e.getClass().getName()) && "org.eclipse.jst.jsf.facelet.ui.FaceletValidationMarker".equals(type)) {
												// Remove WTP's annotations for JBT JSP/XHTML editors.
												annotationsToRemove.add(annotation);
											}
										} catch (CoreException ce) {
											CommonPlugin.getDefault().logError(ce);
										}
									}
								}
//								if(!newAnnotations.isEmpty()) {
//									Annotation[] annotationsToRemoveArray = annotationsToRemove.toArray(new Annotation[annotationsToRemove.size()]);
//									anModel.replaceAnnotations(annotationsToRemoveArray, newAnnotations);
//								}
								for (MarkerAnnotation annotation : annotationsToRemove) {
									anModel.removeAnnotation(annotation);
								}
								for (Annotation annotation : newAnnotations.keySet()) {
									anModel.addAnnotation(annotation, newAnnotations.get(annotation));
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
}
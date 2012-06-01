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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.BadLocationException;
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

	protected abstract String getMessageBundleName();

	public IMessage addMesssage(IFile target, ITextSourceReference location, String preferenceKey, String textMessage, String[] messageArguments) {
		return addMesssage(target, -1, location, preferenceKey, textMessage, messageArguments);
	}

	public IMessage addMesssage(IFile target, int lineNumber, ITextSourceReference location, String preferenceKey, String textMessage, String[] messageArguments) {
		int severity = getSeverity(preferenceKey, target);
		IMessage message = null;
		try {
			if(severity!=-1 && (severity!=IMessage.NORMAL_SEVERITY || !hasSuppressWarningsAnnotation(preferenceKey, location))) {
				message = addMesssage(target, lineNumber, location.getStartPosition(), location.getLength(), severity, preferenceKey, textMessage, messageArguments);
			}
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return message;
	}

	public IMessage addMesssage(IFile target, int offset, int length, String preferenceKey,	String message, String[] messageArguments) {
		return addMesssage(target, -1, offset, length, preferenceKey, message, messageArguments);
	}

	public IMessage addMesssage(IFile target, int lineNumber, int offset, int length, String preferenceKey,	String message, String[] messageArguments) {
		int severity = getSeverity(preferenceKey, target);
		return severity!=-1?addMesssage(target, lineNumber, offset, length, severity, preferenceKey, message, messageArguments):null;
	}

	private IMessage addMesssage(IFile target, int lineNumber, int offset, int length, int severity, String preferenceKey, String textMessage, String[] messageArguments) {
		if(lineNumber<0) {
			try {
				lineNumber = document.getLineOfOffset(offset) + 1;
			} catch (BadLocationException e) {
				CommonPlugin.getDefault().logError(e);
			}
		}
		IMessage message = addMesssage(validationManager, this.reporter, offset, length, target, lineNumber, severity, textMessage, messageArguments, getMessageBundleName());
		return message;
	}

	public static final String AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE = "org.jboss.tools.common.validation.asyoutype";

	private static IMessage addMesssage(IValidator validator, IReporter reporter, int offset, int length, IFile file, int lineNumber, int severity, String textMessage, Object[] messageArguments, String bundleName) {
		Message message = new ValidationMessage(severity, MessageFormat.format(textMessage, messageArguments), file);
		message.setOffset(offset);
		message.setLength(length);
		message.setLineNo(lineNumber);
		message.setBundleName(bundleName);
		message.setAttribute(AS_YOU_TYPE_VALIDATION_ANNOTATION_ATTRIBUTE, Boolean.TRUE);
		reporter.addMessage(validator, message);
		return message;
	}
/*
	static class DisabledAnnotation extends Annotation implements IAnnotationPresentation {

	    private static final int WARNING_LAYER;
	    private static final int ERROR_LAYER;

	    static {
	        AnnotationPreferenceLookup lookup = EditorsUI.getAnnotationPreferenceLookup();
	        WARNING_LAYER = computeLayer("org.eclipse.wst.sse.ui.temp.warning", lookup); //$NON-NLS-1$
	        ERROR_LAYER = computeLayer("org.eclipse.wst.sse.ui.temp.error", lookup); //$NON-NLS-1$
	    }

	    private static int computeLayer(String annotationType, AnnotationPreferenceLookup lookup) {
	        Annotation annotation = new Annotation(annotationType, false, null);
	        AnnotationPreference preference= lookup.getAnnotationPreference(annotation);
	        if (preference != null) {
	            return preference.getPresentationLayer() + 1;
	        } else {
	            return IAnnotationAccessExtension.DEFAULT_LAYER + 1;
	        }
	    }

		public DisabledAnnotation(String type, boolean isPersistent, String text) {
			super(type, isPersistent, text);
		}

		@Override
		public int getLayer() {
	        return WARNING_LAYER;
		}

		@Override
		public void paint(GC gc, Canvas canvas, Rectangle bounds) {
			String path = WorkbenchImages.ICONS_PATH + "dlcl16/showwarn_tsk.gif"; //$NON-NLS-1$
	        URL url = BundleUtility.find(IDEWorkbenchPlugin.IDE_WORKBENCH, path);
	        ImageDescriptor descriptor = ImageDescriptor.createFromURL(url);
			Image image = descriptor.createImage(false);
//			ImageUtilities.drawImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK), gc, canvas, bounds, SWT.CENTER, SWT.TOP);
			ImageUtilities.drawImage(image, gc, canvas, bounds, SWT.CENTER, SWT.TOP);
		}
	}
*/
	protected void disableProblemAnnotations(final ITextSourceReference reference) {
		// Remove (TODO disable) all the existing problem annotations for the reference in case of as-you-type validation
        UIJob job = new UIJob("As-you-type JBT validation. Disabling the marker annotations.") {
			public IStatus runInUIThread(IProgressMonitor monitor) {
				ITextEditor e = EclipseUIUtil.getActiveEditor();
				if(e!=null && e.isDirty()) {
					IEditorInput input = e.getEditorInput();
					IDocumentProvider dp = e.getDocumentProvider();
					if(document == dp.getDocument(input)) {
						IAnnotationModel model = dp.getAnnotationModel(input);
						if(model instanceof AbstractMarkerAnnotationModel) {
							AbstractMarkerAnnotationModel anModel = ((AbstractMarkerAnnotationModel)model);
							synchronized (anModel.getLockObject()) {
								Iterator iterator = anModel.getAnnotationIterator(reference.getStartPosition(), reference.getLength(), true, true);
								Set<MarkerAnnotation> annotationsToRemove = new HashSet<MarkerAnnotation>();
//								Map<Annotation, Position> newAnnotations = new HashMap<Annotation, Position>();
								while (iterator.hasNext()) {
									Object o = iterator.next();
									if(o instanceof MarkerAnnotation) {
										MarkerAnnotation annotation = (MarkerAnnotation)o;
										IMarker marker = annotation.getMarker();
										try {
											String type = marker.getType();
											if(getMarkerType().equals(type)) {
//												Annotation newAnnotation = new DisabledAnnotation(annotation.getType(), false, annotation.getText());
//												int offset = marker.getAttribute(IMarker.CHAR_START, 0);
//												int length = 0; // marker.getAttribute(IMarker.CHAR_END, 0) - offset;
//												Position p = new Position(offset, length);
//												newAnnotations.put(newAnnotation, p);
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
//								for (Annotation annotation : newAnnotations.keySet()) {
//									anModel.addAnnotation(annotation, newAnnotations.get(annotation));
//								}
							}
						}
					}
				}
				return Status.OK_STATUS;
			}
		};
		job.schedule();
	}

	static class ValidationMessage extends Message {

		private String message;

		public ValidationMessage(int severity,	String message, IFile file) {
			super(CommonPlugin.PLUGIN_ID, severity, message, null, file);
			this.message = message;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.wst.validation.internal.core.Message#getText(java.util.Locale, java.lang.ClassLoader)
		 */
		@Override
		public java.lang.String getText(Locale locale, ClassLoader classLoader) {
			return message;
		}
	}
}
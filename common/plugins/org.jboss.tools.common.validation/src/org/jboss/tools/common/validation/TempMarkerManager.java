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
import java.util.Locale;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.wst.validation.internal.core.Message;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.text.ITextSourceReference;

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

	private static IMessage addMesssage(IValidator validator, IReporter reporter, int offset, int length, IFile file, int lineNumber, int severity, String textMessage, Object[] messageArguments, String bundleName) {
		Message message = new ValidationMessage(severity, MessageFormat.format(textMessage, messageArguments), file);
		message.setOffset(offset);
		message.setLength(length);
		message.setLineNo(lineNumber);
		message.setBundleName(bundleName);
		reporter.addMessage(validator, message);
		return message;
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
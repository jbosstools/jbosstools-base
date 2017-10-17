/*******************************************************************************
 * Copyright (c) 2017 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.utils;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.IMessage;

public class MessageFactory {

	public MessageFactory() {
	}

	/**
	 * Returns the message type for a given status severity.
	 * 
	 * @param status
	 * @return
	 * 
	 * @see IMessage#getMessageType()
	 * @see IStatus#getSeverity()
	 */
	private int getMessageType(IStatus status) {
		return getMessageType(status.getSeverity());
	}

	/**
	 * Returns the message type for a given status severity.
	 * @param status severity
	 * @return message type
	 * 
	 * @see IMessage#getMessageType()
	 * @see IStatus#getSeverity()
	 */
	private int getMessageType(int severity) {
		switch(severity) {
			case IStatus.ERROR:
				return IMessage.ERROR;
			case IStatus.WARNING:
				return IMessage.WARNING;
			default:
				return IMessage.NONE;
		}
	}

	/**
	 * Returns an {@link IMessage} for the a given {@link IStatus}. Only message type
	 * and message text are set to the IMessage that is created.
	 * 
	 * @param status
	 * @return
	 * 
	 * @see IMessage
	 * @see IStatus
	 */
	public IMessage toMessage(IStatus status) {
		return create(status.getMessage(), getMessageType(status));
	}

	/**
	 * Returns an {@link IMessage} for given message text and type.
	 * 
	 * @param message
	 * @param type
	 * @return
	 */
	public IMessage create(final String message, final int type) {
		return new IMessage() {
			public Control getControl() { return null; }

			public Object getData() { return null; }

			public Object getKey() { return null; }

			public String getPrefix() { return null; }

			public String getMessage() { return message; }

			public int getMessageType() { return type; }
		};
	}

}

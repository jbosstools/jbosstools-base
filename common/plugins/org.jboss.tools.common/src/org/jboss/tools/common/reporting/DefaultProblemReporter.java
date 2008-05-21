/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.reporting;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;

/**
 * Default implementstion that writes problem to Eclipse log.
 * @author glory
 */
public class DefaultProblemReporter implements IProblemReporter {

	public void reportProblem(IStatus status) {
		if(status != null) Platform.getLog(Platform.getBundle("org.jboss.tools.common")).log(status);
	}

}

/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.tests;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/**
 * @author Alexey Kazakov
 */
public interface IAnnotationTest {

	void assertAnnotationIsCreated(IResource resource, String pattern, int... expectedLines) throws CoreException;
	void assertAnnotationIsCreated(IResource resource, String message, boolean pattern, int... expectedLines) throws CoreException;
	void assertAnnotationIsNotCreated(IResource resource, String message) throws CoreException;
	void assertAnnotationIsNotCreated(IResource resource, String message, int expectedLine) throws CoreException;
	void assertAnnotationIsCreatedForGivenPosition(IResource resource, String message, int lineNumber, int startPosition, int endPosition) throws CoreException;
}
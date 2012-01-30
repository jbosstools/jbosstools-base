/*******************************************************************************
 * Copyright (c) 2007-2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.hyperlink;

import org.eclipse.jface.text.IDocument;

public interface IExclusiblePartitionerRecognition {
	String getExclusionPartitionType();
	boolean excludes(String partitionType, IDocument document, int offset, IHyperlinkRegion superRegion);
}

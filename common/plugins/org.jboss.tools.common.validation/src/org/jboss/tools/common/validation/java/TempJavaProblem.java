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
package org.jboss.tools.common.validation.java;

import java.util.Map;

import org.eclipse.jdt.core.compiler.CategorizedProblem;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.jboss.tools.common.validation.ValidationMessage;

/**
 * @author Alexey Kazakov
 */
public class TempJavaProblem extends CategorizedProblem {
	// TODO for dazarov. Why are you using EL marker type for all the java problems?
	// AsYouType EL Validation 'marker type' name. 
	// marker type is used in the quickFixProcessor extension point
//	public static final String MARKER_TYPE = "org.jboss.tools.common.validation.el"; //$NON-NLS-1$
	public static final String MARKER_TYPE = "TODO";
	
	/** The end offset of the problem */
	private int fSourceEnd= 0;

	/** The line number of the problem */
	private int fLineNumber= 1;

	/** The start offset of the problem */
	private int fSourceStart= 0;

	/** The description of the problem */
	private String fMessage;

	private boolean fIsError;

	/** The originating file name */
	private String fOrigin;
	
	private ValidationMessage vMessage;

	public static final int EL_PROBLEM_ID= 0x88000000;

	/**
	 * Initialize with the given parameters.
	 *
	 * @param message ValidationMessage
	 * @param document the document
	 * @param origin the originating file name
	 */
	public TempJavaProblem(ValidationMessage message, String origin) {
		super();
		fSourceStart= message.getOffset();
		fSourceEnd= message.getOffset() + message.getLength() - 1;
		fLineNumber= message.getLineNumber();
		fMessage= message.getText();
		fOrigin= origin;
		fIsError = (IMessage.NORMAL_SEVERITY != message.getSeverity());
		vMessage = message;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getArguments()
	 */
	public String[] getArguments() {
		return new String[0];
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getID()
	 */
	public int getID() {
		return EL_PROBLEM_ID;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getMessage()
	 */
	public String getMessage() {
		return fMessage;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getOriginatingFileName()
	 */
	public char[] getOriginatingFileName() {
		return fOrigin.toCharArray();
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceEnd()
	 */
	public int getSourceEnd() {
		return fSourceEnd;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceLineNumber()
	 */
	public int getSourceLineNumber() {
		return fLineNumber;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceStart()
	 */
	public int getSourceStart() {
		return fSourceStart;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#isError()
	 */
	public boolean isError() {
		return fIsError;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#isWarning()
	 */
	public boolean isWarning() {
		return !fIsError;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceStart(int)
	 */
	public void setSourceStart(int sourceStart) {
		fSourceStart= sourceStart;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceEnd(int)
	 */
	public void setSourceEnd(int sourceEnd) {
		fSourceEnd= sourceEnd;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceLineNumber(int)
	 */
	public void setSourceLineNumber(int lineNumber) {
		fLineNumber= lineNumber;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.CategorizedProblem#getCategoryID()
	 */
	@Override
	public int getCategoryID() {
		return CAT_SYNTAX;
	}

	/*
	 * @see org.eclipse.jdt.core.compiler.CategorizedProblem#getMarkerType()
	 */
	@Override
	public String getMarkerType() {
		return MARKER_TYPE;
	}
	
	@SuppressWarnings("rawtypes")
	public Map getAttributes(){
		return vMessage.getAttributes();
	}
}
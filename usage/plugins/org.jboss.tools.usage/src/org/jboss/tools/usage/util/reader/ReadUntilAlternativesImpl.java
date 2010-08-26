/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.usage.util.reader;

import java.io.IOException;
import java.io.Reader;

/**
 * @author Andre Dietisheim
 */
public class ReadUntilAlternativesImpl extends ReadUntilImpl {

	private char[][] alternativesCharSequences;
	private char[] alternativeCharacter;
	private int alternativesIndex = -1;

	public ReadUntilAlternativesImpl(Reader reader, String... stringAlternatives) {
		super(reader);
		initAlternativesCharSequences(stringAlternatives);
	}

	private void initAlternativesCharSequences(String... stringAlternatives) {
		this.alternativesCharSequences = new char[stringAlternatives.length][];
		for (int i = 0; i < stringAlternatives.length; i++) {
			this.alternativesCharSequences[i] = stringAlternatives[i].toCharArray();
		}
	}

	protected boolean doContinueRead(char character, int numberOfCharactersRead) throws IOException {
		boolean continueRead = super.doContinueRead(character, numberOfCharactersRead);
		if (!isMatching()) {
			setMatchingIndex(0);
		}
		return continueRead;
	}

	@Override
	protected int getNumberOfCharactersToMatch() {
		if (alternativeCharacter != null) {
			return alternativeCharacter.length;
		} else {
			return 0;
		}
	}

	@Override
	protected boolean doesMatch(char character) {
		if (alternativeCharacter == null || alternativeCharacter[getMatchingIndex()] != character) {
			return matchAlternative(character);
		} else {
			return true;
		}
	}

	private boolean matchAlternative(char character) {
		for (int i = alternativesIndex + 1; i < alternativesCharSequences.length; i++) {
			char[] alternative = alternativesCharSequences[i];
			if (doesMatch(character, alternative)) {
				this.alternativeCharacter = alternative;
				this.alternativesIndex = i;
				return true;
			}

		}
		this.alternativesIndex = -1;
		return false;
	}

	private boolean doesMatch(char character, char[] alternative) {
		for (int j = 0; j <= getMatchingIndex(); j++) {
			if (alternative[j] != character) {
				return false;
			}
		}
		return true;
	}

	@Override
	protected char[] getCharactersToMatch() {
		return alternativeCharacter;
	}

	public String getAlternative() {
		if (alternativesIndex >= 0) {
			return new String(alternativesCharSequences[alternativesIndex]);
		} else {
			return null;
		}
	}
}
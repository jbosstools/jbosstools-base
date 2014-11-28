package org.jboss.tools.tests.tests;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.jboss.tools.tests.tests.IDEPropertiesConstants.knownProperties;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;


/**
 * This test iterates each key,value in a ide-config.properties and validates
 * their values, targets and version are known/sane.
 * 
 * If you see failures reported by this it is either because:
 * 	a) something is inconsistent in ide-config.properties - fix ide-config.properties
 *  b) a valid value have been added that this test does not know about - fix the test.
 *  
 * @author max
 *
 */
@RunWith(Parameterized.class)
public class IDEPropertiesSanityCheckTest {

	/**
	 * Returns each key, value in properties
	 * 
	 * @return
	 * @throws IOException
	 */
	@SuppressWarnings({ "rawtypes" })
	@Parameters(name = "{0}")
	static public Collection<String[]> getIDEProperties() throws IOException {

		URL url = new URL(IDEPropertiesConstants.PROPERTIES_LOCATION);
		InputStream in = url.openStream();
		Reader reader = new InputStreamReader(in, "UTF-8");

		Properties p = new Properties();

		try {
			p.load(reader);
		} finally {
			reader.close();
		}

		List<String[]> result = new ArrayList<String[]>(p.size());

		for (Iterator iterator = p.entrySet().iterator(); iterator.hasNext();) {
			Entry entry = (Entry) iterator.next();
			result.add(new String[] { (String) entry.getKey(),
					(String) entry.getValue() });
		}
		return result;

	}

	@Parameter
	public String rawkey;

	@Parameter(value = 1)
	public String value;

	String key;
	String target;
	String version;

	@Before
	public void setup() {
		String[] elements = rawkey.split("\\|");

		assertTrue("Should minimum have 1 part", elements.length >= 1);

		if (elements.length >= 1) {
			key = elements[0];
		}

		if (elements.length >= 2) {
			target = elements[1];
		}

		if (elements.length >= 3) {
			version = elements[2];
		}

		assertTrue("Should maximum have 3 parts", elements.length <= 3);

	}

	@Test
	public void testIsKnownProperty() {
		assertTrue(key + " is not a known property",
				knownProperties.contains(key));
	}

	@Test
	public void testIsKnownTarget() {
		if (target != null) {
			assertTrue(target + " is not a known target",
					IDEPropertiesConstants.knownTargets.contains(target));
		}
	}

	@SuppressWarnings("unchecked")
	@Test
	public void testIsValidVersion() {
		if (version != null) {
			String[] parts = version.split("\\.");
			assertThat("Version should have between 1 to 4 parts",
					parts.length,
					anyOf(equalTo(1), equalTo(2), equalTo(3), equalTo(4)));
			if (parts.length >= 4) {
				assertTrue(
						parts[3] + " is not a known qualifier",
						IDEPropertiesConstants.knownVersionQualifiers.contains(parts[3]));
			}
		}
	}

}

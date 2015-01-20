package org.jboss.tools.tests.tests;

import static org.hamcrest.CoreMatchers.anyOf;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.jboss.tools.tests.tests.IDEPropertiesConstants.knownProperties;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * This test iterates each key,value in a ide-config.properties and validates
 * their values, targets and version are known/sane.
 * 
 * If you see failures reported by this it is either because: a) something is
 * inconsistent in ide-config.properties - fix ide-config.properties b) a valid
 * value have been added that this test does not know about - fix the test.
 * 
 * @author max
 *
 */
@RunWith(Parameterized.class)
public class IDEPropertiesSanityTest {

	/**
	 * Returns each key, value in properties
	 * 
	 * @return
	 * @throws IOException
	 */
	@SuppressWarnings({ "rawtypes" })
	@Parameters(name = "{0}")
	static public Collection<String[]> getIDEProperties() throws IOException {

		Map rawProperties = loadIDEProperties();
		
		List<String[]> result = new ArrayList<String[]>(rawProperties.size());

		for (Iterator iterator = rawProperties.entrySet().iterator(); iterator
				.hasNext();) {
			Entry entry = (Entry) iterator.next();
			result.add(new String[] { (String) entry.getKey(),
					(String) entry.getValue() });
		}
		return result;

	}

	static Set<String> unseenKeys = new HashSet<String>();

	@BeforeClass
	static public void setupAllKnownProperties() {
		unseenKeys.addAll(IDEPropertiesConstants.knownProperties);
	}

	@AfterClass
	static public void checkAllPropertiesBeenSeen() {
		// assertThat(unseenKeys, Matchers.)
		assertEquals("Did not see " + unseenKeys, unseenKeys.isEmpty(), true);
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

		unseenKeys.remove(key);
	}

	@Test
	public void testIsKnownTarget() {
		if (target != null) {
			assertTrue(target + " is not a known target",
					IDEPropertiesConstants.knownTargets.contains(target));
		}
	}

	@Test
	public void testIsValidVersion() {
		if (version != null) {
			String[] parts = version.split("\\.");
			assertThat("Version should have between 1 to 4 parts",
					parts.length,
					anyOf(equalTo(1), equalTo(2), equalTo(3), equalTo(4)));
			if (parts.length >= 4) {
				assertThat("Qualifier is not known",
						IDEPropertiesConstants.knownVersionQualifiers,
						hasItem(parts[3]));
			}
		}
	}

	static public Map loadIDEProperties() throws IOException {
		URL url = new URL(IDEPropertiesConstants.PROPERTIES_LOCATION);
		Properties rawProperties = new Properties();
		try (InputStream in = url.openStream()) {
			Reader reader = new InputStreamReader(in, "UTF-8");
			rawProperties.load(reader);
		}
		return rawProperties;
	}
}

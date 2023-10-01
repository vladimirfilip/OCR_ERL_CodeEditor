from unittest import TestCase

from parsed_token import TokenVals, ParsedToken, TokenContents


class TestParsedToken(TestCase):

    def __init__(self, method_name='runTest'):
        super().__init__(method_name)
        self.plus = ParsedToken().set_val(TokenVals.PLUS)

    def test_text_getter(self):
        self.assertEqual('+', self.plus.text)
        self.assertEqual(TokenContents.PLUS, self.plus.content)
        self.assertEqual(TokenVals.PLUS, self.plus.val)

    def test_text_setter(self):
        #
        # Set to another recognized text
        #
        self.plus.text = TokenContents.EQUALS.value
        self.assertEqual('=', self.plus.text)
        self.assertEqual(TokenContents.EQUALS, self.plus.content)
        self.assertEqual(TokenVals.EQUALS, self.plus.val)
        #
        # Set now an identifier, check it changes Content but not Val
        #
        self.plus.text = 'some_id'
        self.assertEqual('some_id', self.plus.text)
        self.assertIsNone(self.plus.content)
        self.assertEqual(TokenVals.EQUALS, self.plus.val)

    def test_content_getter(self):
        self.assertEqual(TokenContents.PLUS, self.plus.content)
        self.assertEqual(TokenVals.PLUS, self.plus.val)
        self.assertEqual('+', self.plus.text)

    def test_content_setter(self):
        #
        # Set to COMMA and see that the other fields get set, too
        #
        self.plus.content = TokenContents.COMMA
        self.assertEqual(TokenContents.COMMA, self.plus.content)
        self.assertEqual(TokenVals.COMMA, self.plus.val)
        self.assertEqual(',', self.plus.text)
        #
        # Set to None and see that the other fields remain untouched
        #
        self.plus.content = None
        self.assertIsNone(self.plus.content)
        self.assertEqual(TokenVals.COMMA, self.plus.val)
        self.assertEqual(',', self.plus.text)

    def test_val_getter(self):
        self.assertEqual(TokenVals.PLUS, self.plus.val)
        self.assertEqual(TokenContents.PLUS, self.plus.content)
        self.assertEqual('+', self.plus.text)

    def test_val_setter(self):
        #
        # Set to Val that has no text, see the other fields remain unchanged
        #
        self.plus.val = TokenVals.ID
        self.assertEqual(TokenContents.PLUS, self.plus.content)
        self.assertEqual(TokenVals.ID, self.plus.val)
        self.assertEqual('+', self.plus.text)
        #
        # Set to known Val, see the other fields get changed, too
        #
        self.plus.val = TokenVals.OPEN_PAREN
        self.assertEqual(TokenContents.OPEN_PAREN, self.plus.content)
        self.assertEqual(TokenVals.OPEN_PAREN, self.plus.val)
        self.assertEqual('(', self.plus.text)

    def test_set_text(self):
        t: ParsedToken = self.plus.set_text('some_id').set_val(TokenVals.ID)
        self.assertIs(self.plus, t)
        self.assertEqual('some_id', t.text)
        self.assertEqual(TokenVals.ID, t.val)
        self.assertIsNone(t.content)

    #
    # Only testing that chaining setters modifies test token in place and returns the same token from now on,
    # as functionality with data being properly written has been already tested
    #
    def test_set_content(self):
        t: ParsedToken = self.plus.set_content(TokenContents.EQUALS)
        self.assertEqual(TokenVals.EQUALS, self.plus.val)
        self.assertEqual('=', self.plus.text)
        self.assertIs(t, self.plus)

        t = self.plus.set_content(TokenContents.COMMA).set_content(None)
        self.assertIs(t, self.plus)
        self.assertEqual(TokenVals.COMMA, self.plus.val)
        self.assertEqual(',', self.plus.text)
        self.assertIsNone(self.plus.content)

    def test_set_val(self):
        t: ParsedToken = self.plus.set_val(TokenVals.ID).set_text("some id")
        self.assertIsNone(self.plus.content)
        self.assertEqual("some id", self.plus.text)
        self.assertIs(t, self.plus)
